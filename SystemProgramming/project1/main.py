"""
x86-32 Assembly Code Editor - Entry Point
Launches the tkinter GUI application.
"""

import sys
import tkinter as tk
from editor import AssemblyEditor


def main():
    root = tk.Tk()
    app = AssemblyEditor(root)

    # Open file from command line argument if provided
    if len(sys.argv) > 1:
        app.open_file_from_arg(sys.argv[1])

    root.mainloop()


if __name__ == '__main__':
    main()
