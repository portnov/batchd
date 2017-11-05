import bpy
from bpy.types import WindowManager, AddonPreferences
from bpy.props import StringProperty, EnumProperty

from batchd import client

batchd_client = None
batchd_queues = []
batchd_types = []

def get_preferences():
    return bpy.context.user_preferences.addons.get("batchd").preferences

def get_batchd_client(context):
    global batchd_client

    if batchd_client is not None:
        return batchd_client

    addon = get_preferences()
    batchd_client = client.Client(addon.manager_url, addon.username, addon.password)
    return batchd_client

def queues_from_batchd(self, context):
    global batchd_queues

    if len(batchd_queues) > 0 or context is None:
        return batchd_queues

    c = get_batchd_client(context)
    for queue in c.get_queues():
        name = queue.get('name', None)
        title = queue.get('title', name)
        batchd_queues.append((name, title, title))

    return batchd_queues

def types_from_batchd(self, context):
    global batchd_types

    if len(batchd_types) > 0 or context is None:
        print("types: {}, context: {}".format(batchd_types, context))
        return batchd_types

    c = get_batchd_client(context)
    for type in c.get_job_types():
        name = type.get('name')
        title = type.get('title', name)
        if not title:
            title = name
        batchd_types.append((name, title, title))

    print(batchd_types)
    return batchd_types

class SettingsPanel(bpy.types.AddonPreferences):
    bl_label = "Batchd settings"
    bl_idname = __package__

    manager_url = StringProperty(
            name = "batchd manager URL",
            default = "http://localhost:9681")

    batchd_queue = EnumProperty(name="Queue", items = queues_from_batchd)
    job_type_name = EnumProperty(name="batchd job type", items = types_from_batchd)
    username = StringProperty(name="batchd user name")
    password = StringProperty(name="batchd password", subtype="PASSWORD")

    def draw(self, context):
        layout = self.layout

        layout.prop(self, "manager_url")
        layout.prop(self, "username")
        layout.prop(self, "password")
        layout.prop(self, "batchd_queue")
        layout.prop(self, "job_type_name")

class EnqueuePanel(bpy.types.Panel):
    bl_label = "Submit to batchd"
    bl_idname = "batchd.enqueue.panel"
    bl_space_type = "PROPERTIES"
    bl_context = "render"
    bl_region_type = "WINDOW"

    def draw(self, context):
        layout = self.layout
        wm = context.window_manager

        layout.operator("batchd.enqueue")

class EnqueueOperator(bpy.types.Operator):
    bl_label = "Submit to batchd"
    bl_idname = "batchd.enqueue"

    def execute(self, context):
        wm = context.window_manager

        bpy.ops.file.pack_all()
        current_file = bpy.data.filepath
        target_file = bpy.path.abspath(bpy.context.scene.render.filepath)

        job_type_name = get_preferences().job_type_name
        queue_name = get_preferences().batchd_queue

        c = get_batchd_client(context)
        params = dict(input=current_file, output=target_file, frame="1")
        c.do_enqueue(queue_name, job_type_name, params)

        return {'FINISHED'}

def register():
    bpy.utils.register_class(SettingsPanel)
    bpy.utils.register_class(EnqueueOperator)
    bpy.utils.register_class(EnqueuePanel)

def unregister():
    bpy.utils.unregister_class(EnqueuePanel)
    bpy.utils.unregister_class(EnqueueOperator)
    bpy.utils.unregister_class(SettingsPanel)

if __name__ == "__main__":
    register()

