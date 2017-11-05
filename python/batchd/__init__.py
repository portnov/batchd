bl_info = {
        "name": "Batchd Client",
        "author": "portnov84@rambler.ru",
        "version": (0, 0, 0, 1),
        "blender": (2, 7, 8),
        "location": "?",
        "description": "Batchd client for Blender",
        "warning": "",
        "wiki_url": "https://github.com/portnov/batchd/wiki",
        "tracker_url": "https://github.com/portnov/batchd/issues",
        "category": "Object"
        }

import bpy
import importlib

from . import blenderclient

def register():
    blenderclient.register()

def unregister():
    blenderclient.unregister()

if __name__ == "__main__":
    register()

