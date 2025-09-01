import tkinter as tk
from PIL import Image, ImageTk, ImageDraw
import threading, time, math

class AuroraProgress:
    def __init__(self, image_path):
        self.root = tk.Tk()
        self.root.title("Silverstarr Build Progress")
        self.canvas = tk.Canvas(self.root, width=300, height=300, bg='black', highlightthickness=0)
        self.canvas.pack()
        self.base_img = Image.open(image_path).resize((300, 300), Image.ANTIALIAS)
        self.tk_img = ImageTk.PhotoImage(self.base_img)
        self.canvas.create_image(150, 150, image=self.tk_img)
        self.arc = None
        self.label = self.canvas.create_text(150, 270, text="Initializing...", fill="white", font=("Segoe UI", 12))
        self.percent = 0
        self.phase = "Start"
        self.running = True
        threading.Thread(target=self.animate, daemon=True).start()

    def update(self, percent, phase):
        self.percent = percent
        self.phase = phase

    def animate(self):
        while self.running:
            img = self.base_img.copy()
            draw = ImageDraw.Draw(img)
            angle = int(360 * self.percent)
            draw.pieslice([30, 30, 270, 270], -90, -90 + angle, fill=(255, 255, 255, 128))
            self.tk_img = ImageTk.PhotoImage(img)
            self.canvas.create_image(150, 150, image=self.tk_img)
            self.canvas.itemconfig(self.label, text=f"{self.phase} {int(self.percent*100)}%")
            self.root.update()
            time.sleep(0.05)

    def stop(self):
        self.running = False
        self.root.destroy()

def launch_gui(image_path):
    gui = AuroraProgress(image_path)
    return gui
