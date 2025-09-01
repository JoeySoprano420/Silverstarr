import time

class DummyGUI:
    def __init__(self, image_path):
        print(f"[aurora_gui] GUI started with image: {image_path}")

    def update(self, progress, message):
        print(f"[aurora_gui] Progress: {progress*100:.0f}% - {message}")

    def stop(self):
        print("[aurora_gui] GUI stopped.")

def launch_gui(image_path):
    return DummyGUI(image_path)

