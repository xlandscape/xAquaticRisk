#!/usr/bin/env python3
"""
folder_picker.py  –  Cross-platform native folder-selection dialog.

Prints the selected path to stdout (empty string if cancelled).
Usage:  python folder_picker.py [initial_dir]

Primary:  wxPython  (installed via controlpanel/requirements.txt)
Fallback: tkinter   (stdlib, available on most system Python installations)
"""
import sys


def _pick_wx(initial: str) -> str:
    import wx
    app = wx.App(False)
    dlg = wx.DirDialog(
        None,
        "Select folder containing xrun files",
        defaultPath=initial,
        style=wx.DD_DEFAULT_STYLE | wx.DD_DIR_MUST_EXIST,
    )
    path = dlg.GetPath() if dlg.ShowModal() == wx.ID_OK else ""
    dlg.Destroy()
    app.Destroy()
    return path


def _pick_tkinter(initial: str) -> str:
    import tkinter as tk
    from tkinter import filedialog
    root = tk.Tk()
    root.withdraw()
    root.attributes("-topmost", True)
    path = filedialog.askdirectory(
        title="Select folder containing xrun files",
        initialdir=initial,
    )
    root.destroy()
    return path or ""


def main() -> None:
    initial = sys.argv[1] if len(sys.argv) > 1 else "."
    path = ""
    try:
        path = _pick_wx(initial)
    except ImportError:
        try:
            path = _pick_tkinter(initial)
        except Exception:
            pass
    print(path)


if __name__ == "__main__":
    main()
