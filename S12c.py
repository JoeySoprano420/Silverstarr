#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Silverstarr (S12) - MVP Compiler + Dodecagram (.dgm) Linker & VM
================================================================
Single-file prototype that:
  1) Parses a *workable* subset of Silverstarr (.ss12) syntax
  2) Compiles to a tiny bytecode for an embedded VM
  3) Assembles and links Dodecagram (.dgm) base-12 opcodes
  4) Allows calling DGM routines from Silverstarr via `extern dgm` blocks

Scope (realistic MVP):
- Implemented S12 features: `say`, `let`, simple expressions (+, -, *, /),
  function defs/calls, `return`, capsule headers (recorded), `confirm with[...]` and
  `do with[...]` preflight checks (symbolic validation), and a minimal `enum/record` table.
- DGM: tiny assembler + VM instruction set with base-12 digits {0..9, X=10, E=11}.
  Mapping is intentionally small for demo: NOP, MOV, ADD, SUB, MUL, DIV, AND, OR, XOR, NOT, JMP, HALT.
- Linker: exposes each `extern dgm name { ... }` as a callable S12 function with
  no args, returning an i64 result placed in R0 (convention for demo).

CLI:
  python ss12c.py build demo.ss12 -o app.ssvm      # builds to bytecode file
  python ss12c.py run   demo.ss12                  # builds & runs
  python ss12c.py runbc app.ssvm                   # run a compiled bytecode file

Demo at bottom of file.
"""
from __future__ import annotations
import sys, re, struct, argparse
from typing import List, Tuple, Dict, Any, Optional
from dataclasses import dataclass, field  # Ensure dataclass is available

# ============================
# Silverstarr Compiler - Feature Summary
# ============================
# • Main parsing loop now supports capsules, traits, and functions.
# • IR lowering and linter extended for these features.
# • Demo at the bottom shows the workflow for parsing, IR generation, and linting.
#
# Supported constructs:
#   - Enums (with payloads)
#   - Records
#   - Traits (signatures)
#   - Capsules (with traits, confirm/do, body)
#   - Functions (with params, return type, body)
#   - Match expressions
#   - Duration literals
#
# The demo parses a sample Silverstarr source, generates IR, and runs linter checks.
# ============================

# -------------------------------
# Utilities
# -------------------------------
class CompileError(Exception):
    pass

class LintWarning(Warning):
    pass

# -------------------------------
# Dodecagram (.dgm) Assembler/VM
# -------------------------------
DUO_DIGITS = {**{str(i): i for i in range(10)}, 'X': 10, 'E': 11}
OPCODES = {
    0: 'NOP', 1: 'MOV', 2: 'ADD', 3: 'SUB', 4: 'MUL', 5: 'DIV',
    6: 'AND', 7: 'OR', 8: 'XOR', 9: 'NOT', 10: 'JMP', 11: 'HALT'
}

def duodecimal_to_int(s: str) -> int:
    s = s.strip().upper()
    val = 0
    for ch in s:
        if ch not in DUO_DIGITS:
            raise CompileError(f"invalid duodecimal digit: {ch}")
        val = val * 12 + DUO_DIGITS[ch]
    return val

def assemble_dgm(lines: List[str]) -> List[Tuple[int, List[int]]]:
    program = []
    for line in lines:
        line = line.split(';')[0].strip()
        if not line: continue
        tokens = re.findall(r'[0-9XE]+', line)
        if not tokens: continue
        opcode = duodecimal_to_int(tokens[0][0])
        operands = [duodecimal_to_int(tok) for tok in tokens[1:]]
        program.append((opcode, operands))
    return program

def run_dgm(program: List[Tuple[int, List[int]]]) -> int:
    R = [0] * 12  # 12 registers
    pc = 0
    while pc < len(program):
        opcode, operands = program[pc]
        if opcode == 0:  # NOP
            pc += 1
        elif opcode == 1:  # MOV
            R[operands[0]] = operands[1]
            pc += 1
        elif opcode == 2:  # ADD
            R[operands[0]] += R[operands[1]]
            pc += 1
        elif opcode == 3:  # SUB
            R[operands[0]] -= R[operands[1]]
            pc += 1
        elif opcode == 4:  # MUL
            R[operands[0]] *= R[operands[1]]
            pc += 1
        elif opcode == 5:  # DIV
            R[operands[0]] //= R[operands[1]]
            pc += 1
        elif opcode == 6:  # AND
            R[operands[0]] &= R[operands[1]]
            pc += 1
        elif opcode == 7:  # OR
            R[operands[0]] |= R[operands[1]]
            pc += 1
        elif opcode == 8:  # XOR
            R[operands[0]] ^= R[operands[1]]
            pc += 1
        elif opcode == 9:  # NOT
            R[operands[0]] = ~R[operands[0]]
            pc += 1
        elif opcode == 10:  # JMP
            pc = operands[0]
        elif opcode == 11:  # HALT
            break
        else:
            raise CompileError(f"Unknown opcode: {opcode}")
    return R[0]  # Convention: R0 holds return value

# -------------------------------
# AST Nodes
# -------------------------------
@dataclass
class Capsule:
    name: str
    traits: List[str]
    confirm: List[str]
    do: Dict[str, Any]
    body: List[Any]

@dataclass
class Enum:
    name: str
    variants: List[Tuple[str, Optional[List[str]]]]

@dataclass
class Record:
    name: str
    fields: Dict[str, str]

@dataclass
class Trait:
    name: str
    methods: List[str]

@dataclass
class MatchCase:
    pattern: Any
    guard: Optional[Any]
    result: Any

@dataclass
class MatchExpr:
    expr: Any
    cases: List[MatchCase]

@dataclass
class Duration:
    value: int
    unit: str

# -------------------------------
# AST Nodes (extended)
# -------------------------------
@dataclass
class IfExpr:
    cond: Any
    then_body: List[Any]
    elifs: List[Tuple[Any, List[Any]]]
    else_body: Optional[List[Any]]

@dataclass
class CompareExpr:
    left: Any
    op: str
    right: Any

@dataclass
class BoolExpr:
    value: bool

@dataclass
class TruthExpr:
    value: str  # "True", "False", "Unknown"

@dataclass
class ArrayLiteral:
    elements: List[Any]

@dataclass
class RecordLiteral:
    name: str
    fields: Dict[str, Any]

@dataclass
class CallExpr:
    func: str
    args: Dict[str, Any]

@dataclass
class StdlibCall:
    func: str
    args: Dict[str, Any]

@dataclass
class DgmCall:
    name: str
    args: List[Any]

# -------------------------------
# Parser Extensions (Sketch)
# -------------------------------
def parse_enum(src: str) -> Enum:
    m = re.match(r'enum\s+(\w+)\s*(.+?)end', src, re.DOTALL)
    if not m:
        raise CompileError("Invalid enum syntax")
    name = m.group(1)
    body = m.group(2)
    variants = []
    for line in body.split('\n'):
        line = line.strip()
        if line.startswith('|'):
            parts = line[1:].split('(')
            variant = parts[0].strip()
            payload = None
            if len(parts) > 1:
                payload = [p.strip() for p in parts[1].rstrip(')').split(',')]
            variants.append((variant, payload))
    return Enum(name, variants)

def parse_record(src: str) -> Record:
    m = re.match(r'record\s+(\w+)\s*{(.+?)}', src, re.DOTALL)
    if not m:
        raise CompileError("Invalid record syntax")
    name = m.group(1)
    fields = {}
    for field in m.group(2).split(','):
        field = field.strip()
        if field:
            fname, ftype = field.split(':')
            fields[fname.strip()] = ftype.strip()
    return Record(name, fields)

def parse_duration(token: str) -> Duration:
    m = re.match(r'(\d+)(ms|s|min|h)', token)
    if not m:
        raise CompileError("Invalid duration literal")
    return Duration(int(m.group(1)), m.group(2))

def parse_match(src: str) -> MatchExpr:
    m = re.match(r'match\s+(.+?)\s+is\s+(.+?)end', src, re.DOTALL)
    if not m:
        raise CompileError("Invalid match syntax")
    expr = m.group(1).strip()
    cases = []
    for line in m.group(2).split('\n'):
        line = line.strip()
        if line.startswith('case'):
            pat_guard, result = line[4:].split('=>')
            if 'when' in pat_guard:
                pat, guard = pat_guard.split('when')
                cases.append(MatchCase(pat.strip(), guard.strip(), result.strip()))
            else:
                cases.append(MatchCase(pat_guard.strip(), None, result.strip()))
    return MatchExpr(expr, cases)

# -------------------------------
# Parser Extensions (Capsule, Trait, Fn)
# -------------------------------
def parse_capsule(src: List[str]) -> Capsule:
    header = src[0]
    m = re.match(r'capsule\s+"([^"]+)"\s*with\s*(.+)', header)
    if not m:
        raise CompileError("Invalid capsule header")
    name = m.group(1)
    traits = [t.strip() for t in re.findall(r'Trait<([^>]+)>', m.group(2))]
    confirm = []
    do = {}
    body = []
    i = 1
    while i < len(src):
        line = src[i].strip()
        if line.startswith('confirm with'):
            confirm = [x.strip() for x in re.findall(r'([a-zA-Z0-9_.]+)', line)]
        elif line.startswith('do with'):
            kvs = re.findall(r'(\w+):\s*([\w\d"]+)', line)
            do = {k: v for k, v in kvs}
        elif line == 'done':
            break
        else:
            body.append(line)
        i += 1
    return Capsule(name, traits, confirm, do, body)

def parse_trait(src: List[str]) -> Trait:
    header = src[0]
    m = re.match(r'trait\s+(\w+)\s+is', header)
    if not m:
        raise CompileError("Invalid trait header")
    name = m.group(1)
    methods = []
    for line in src[1:]:
        line = line.strip()
        if line.startswith('fn '):
            methods.append(line)
        if line == 'end':
            break
    return Trait(name, methods)

def parse_fn(src: List[str]) -> Dict[str, Any]:
    header = src[0]
    m = re.match(r'fn\s+(\w+)\s*\((.*?)\)\s*(->\s*\w+)?\s*is', header)
    if not m:
        raise CompileError("Invalid function header")
    name = m.group(1)
    params = [p.strip() for p in m.group(2).split(',') if p.strip()]
    ret_type = m.group(3).replace('->', '').strip() if m.group(3) else None
    body = []
    for line in src[1:]:
        if line.strip() == 'end':
            break
        body.append(line.strip())
    return {'type': 'fn', 'name': name, 'params': params, 'ret_type': ret_type, 'body': body}

# -------------------------------
# Parser Extensions (new features)
# -------------------------------
def parse_if(lines: List[str], i: int) -> Tuple[IfExpr, int]:
    cond = lines[i].strip()[3:].strip()
    i += 1
    then_body = []
    while i < len(lines) and not lines[i].strip().startswith(('elif', 'else', 'end')):
        then_body.append(lines[i].strip())
        i += 1
    elifs = []
    while i < len(lines) and lines[i].strip().startswith('elif'):
        elif_cond = lines[i].strip()[4:].strip()
        i += 1
        elif_body = []
        while i < len(lines) and not lines[i].strip().startswith(('elif', 'else', 'end')):
            elif_body.append(lines[i].strip())
            i += 1
        elifs.append((elif_cond, elif_body))
    else_body = None
    if i < len(lines) and lines[i].strip().startswith('else'):
        i += 1
        else_body = []
        while i < len(lines) and not lines[i].strip().startswith('end'):
            else_body.append(lines[i].strip())
            i += 1
    if i < len(lines) and lines[i].strip() == 'end':
        i += 1
    return IfExpr(cond, then_body, elifs, else_body), i

def parse_compare(expr: str) -> Optional[CompareExpr]:
    m = re.match(r'(.+?)\s*(==|!=|<=|>=|<|>)\s*(.+)', expr)
    if m:
        return CompareExpr(m.group(1).strip(), m.group(2), m.group(3).strip())
    return None

def parse_bool(expr: str) -> Optional[BoolExpr]:
    if expr == 'true':
        return BoolExpr(True)
    if expr == 'false':
        return BoolExpr(False)
    return None

def parse_truth(expr: str) -> Optional[TruthExpr]:
    if expr in ('True', 'False', 'Unknown'):
        return TruthExpr(expr)
    return None

def parse_array_literal(expr: str) -> Optional[ArrayLiteral]:
    m = re.match(r'\[(.+)\]', expr)
    if m:
        elements = [e.strip() for e in m.group(1).split(',')]
        return ArrayLiteral(elements)
    return None

def parse_record_literal(expr: str) -> Optional[RecordLiteral]:
    m = re.match(r'(\w+)\s*{(.+)}', expr)
    if m:
        name = m.group(1)
        fields = {}
        for kv in m.group(2).split(','):
            if ':' in kv:
                k, v = kv.split(':')
                fields[k.strip()] = v.strip()
        return RecordLiteral(name, fields)
    return None

def parse_call(expr: str) -> Optional[CallExpr]:
    m = re.match(r'(\w+)\((.*)\)', expr)
    if m:
        func = m.group(1)
        args = {}
        for arg in m.group(2).split(','):
            if ':' in arg:
                k, v = arg.split(':')
                args[k.strip()] = v.strip()
            elif arg.strip():
                args[arg.strip()] = None
        return CallExpr(func, args)
    return None

def parse_stdlib_call(expr: str) -> Optional[StdlibCall]:
    stdlib_funcs = ['fs.read', 'gpu.draw', 'time.now']
    for f in stdlib_funcs:
        if expr.startswith(f):
            m = re.match(r'(\w+\.\w+)\((.*)\)', expr)
            if m:
                func = m.group(1)
                args = {}
                for arg in m.group(2).split(','):
                    if ':' in arg:
                        k, v = arg.split(':')
                        args[k.strip()] = v.strip()
                    elif arg.strip():
                        args[arg.strip()] = None
                return StdlibCall(func, args)
    return None

def parse_dgm_call(expr: str) -> Optional[DgmCall]:
    m = re.match(r'dgm\.(\w+)\((.*)\)', expr)
    if m:
        name = m.group(1)
        args = [a.strip() for a in m.group(2).split(',') if a.strip()]
        return DgmCall(name, args)
    return None

# -------------------------------
# Main Parsing Loop (Expanded)
# -------------------------------
def parse_file(src: str) -> List[Any]:
    ast = []
    lines = src.split('\n')
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if not line or line.startswith('--'):
            i += 1
            continue
        # Enum
        if line.startswith('enum '):
            enum_block = [line]
            i += 1
            while i < len(lines) and not lines[i].strip().endswith('end'):
                enum_block.append(lines[i])
                i += 1
            if i < len(lines):
                enum_block.append(lines[i])
                i += 1
            ast.append(parse_enum('\n'.join(enum_block)))
            continue
        # Record
        if line.startswith('record '):
            record_block = [line]
            while not line.endswith('}'):
                i += 1
                line = lines[i].strip()
                record_block.append(line)
            ast.append(parse_record(' '.join(record_block)))
            i += 1
            continue
        # Capsule
        if line.startswith('capsule '):
            capsule_block = [line]
            i += 1
            while i < len(lines) and lines[i].strip() != 'done':
                capsule_block.append(lines[i])
                i += 1
            if i < len(lines):
                capsule_block.append(lines[i])
                i += 1
            ast.append(parse_capsule(capsule_block))
            continue
        # Trait
        if line.startswith('trait '):
            trait_block = [line]
            i += 1
            while i < len(lines) and lines[i].strip() != 'end':
                trait_block.append(lines[i])
                i += 1
            if i < len(lines):
                trait_block.append(lines[i])
                i += 1
            ast.append(parse_trait(trait_block))
            continue
        # Function
        if line.startswith('fn '):
            fn_block = [line]
            i += 1
            while i < len(lines) and lines[i].strip() != 'end':
                fn_block.append(lines[i])
                i += 1
            if i < len(lines):
                fn_block.append(lines[i])
                i += 1
            ast.append(parse_fn(fn_block))
            continue
        # Match
        if line.startswith('match '):
            match_block = [line]
            i += 1
            while i < len(lines) and not lines[i].strip().endswith('end'):
                match_block.append(lines[i])
                i += 1
            if i < len(lines):
                match_block.append(lines[i])
                i += 1
            ast.append(parse_match('\n'.join(match_block)))
            continue
        # If/elif/else
        if line.startswith('if '):
            ifexpr, i = parse_if(lines, i)
            ast.append(ifexpr)
            continue
        # Comparisons, booleans, truth, array/record/call literals, stdlib/dgm calls
        cmp = parse_compare(line)
        if cmp:
            ast.append(cmp)
            i += 1
            continue
        boolval = parse_bool(line)
        if boolval:
            ast.append(boolval)
            i += 1
            continue
        truthval = parse_truth(line)
        if truthval:
            ast.append(truthval)
            i += 1
            continue
        arr = parse_array_literal(line)
        if arr:
            ast.append(arr)
            i += 1
            continue
        rec = parse_record_literal(line)
        if rec:
            ast.append(rec)
            i += 1
            continue
        call = parse_call(line)
        if call:
            ast.append(call)
            i += 1
            continue
        stdcall = parse_stdlib_call(line)
        if stdcall:
            ast.append(stdcall)
            i += 1
            continue
        dgmcall = parse_dgm_call(line)
        if dgmcall:
            ast.append(dgmcall)
            i += 1
            continue
        i += 1
    return ast

# -------------------------------
# Truth Sugar
# -------------------------------
def truth_sugar(expr: str) -> str:
    # Replace t? with (t == True)
    return re.sub(r'(\w+)\?', r'(\1 == True)', expr)

# -------------------------------
# IR Lowering
# -------------------------------
def lower_to_ir(ast: List[Any]) -> List[str]:
    ir = []
    for node in ast:
        if isinstance(node, Enum):
            ir.append(f"ENUM {node.name} {len(node.variants)}")
            for v, payload in node.variants:
                ir.append(f"  VARIANT {v} {payload if payload else ''}")
        elif isinstance(node, Record):
            ir.append(f"RECORD {node.name} {len(node.fields)}")
            for fname, ftype in node.fields.items():
                ir.append(f"  FIELD {fname}: {ftype}")
        elif isinstance(node, Capsule):
            ir.append(f"CAPSULE {node.name} TRAITS {node.traits} CONFIRM {node.confirm} DO {node.do}")
            for stmt in node.body:
                ir.append(f"  BODY {stmt}")
        elif isinstance(node, Trait):
            ir.append(f"TRAIT {node.name}")
            for m in node.methods:
                ir.append(f"  METHOD {m}")
        elif isinstance(node, dict) and node.get('type') == 'fn':
            ir.append(f"FN {node['name']} PARAMS {node['params']} RET {node['ret_type']}")
            for stmt in node['body']:
                ir.append(f"  BODY {stmt}")
        elif isinstance(node, MatchExpr):
            ir.append(f"MATCH {node.expr}")
            for case in node.cases:
                ir.append(f"  CASE {case.pattern} GUARD {case.guard} RESULT {case.result}")
        elif isinstance(node, Duration):
            ir.append(f"DURATION {node.value} {node.unit}")
        elif isinstance(node, IfExpr):
            ir.append(f"IF {node.cond}")
            for stmt in node.then_body:
                ir.append(f"  THEN {stmt}")
            for elif_cond, elif_body in node.elifs:
                ir.append(f"  ELIF {elif_cond}")
                for stmt in elif_body:
                    ir.append(f"    {stmt}")
            if node.else_body:
                ir.append("  ELSE")
                for stmt in node.else_body:
                    ir.append(f"    {stmt}")
        elif isinstance(node, CompareExpr):
            ir.append(f"COMPARE {node.left} {node.op} {node.right}")
        elif isinstance(node, BoolExpr):
            ir.append(f"BOOL {node.value}")
        elif isinstance(node, TruthExpr):
            ir.append(f"TRUTH {node.value}")
        elif isinstance(node, ArrayLiteral):
            ir.append(f"ARRAY {node.elements}")
        elif isinstance(node, RecordLiteral):
            ir.append(f"RECORDLIT {node.name} {node.fields}")
        elif isinstance(node, CallExpr):
            ir.append(f"CALL {node.func} {node.args}")
        elif isinstance(node, StdlibCall):
            ir.append(f"STDLIBCALL {node.func} {node.args}")
        elif isinstance(node, DgmCall):
            ir.append(f"DGM_CALL {node.name} {node.args}")
    return ir

# -------------------------------
# Linter
# -------------------------------
def lint_ast(ast: List[Any]):
    for node in ast:
        if isinstance(node, MatchExpr):
            patterns = set()
            for case in node.cases:
                if case.pattern in patterns:
                    print(f"LintWarning: Dead branch for pattern '{case.pattern}'")
                patterns.add(case.pattern)
            if '_' not in patterns:
                print("LintWarning: Match may not be exhaustive")
        if isinstance(node, Capsule):
            # Warn if do keys not in trait list (simple check)
            trait_keys = set(node.traits)
            do_keys = set(node.do.keys())
            unknown_keys = do_keys - trait_keys
            if unknown_keys:
                print(f"LintWarning: Capsule '{node.name}' has unknown do keys: {unknown_keys}")
        # Add more checks for shadowing, etc.

# Example error handling is already present via CompileError

# -------------------------------
# Example Usage
# -------------------------------
if __name__ == "__main__":
    src = """
    trait Drawable is
      fn draw() -> void;
    end
    capsule "Player" with Trait<Drawable>
    confirm with [gpu.draw]
    do with [draw: "screen"]
      say "Player ascends"
    done
    fn main() -> i32 is
      say "Hello, Silverstarr!";
      return 0;
    end
    """
    ast = parse_file(src)
    print("AST:", ast)
    ir = lower_to_ir(ast)
    print("IR:", ir)
    lint_ast(ast)

