from SCons.Script import *

import os

from commons import *

def _ada_commands_generator(target, source, env, for_signature):
    main_source = os.path.splitext(env['ADA_MAIN'])[0]
    out_dir = without_top_directory(os.path.dirname(main_source), n=2)

    ada_flags = ' '.join(['-I"' + dir_ + '"' for dir_ in env['INCLUDE']])
    compile_cmd = (["gnatmake -c '{}' {} -i".format(
                     main_source,
                     ada_flags)]
                  + ["gnatbind '{}' {}".format(
                      main_source,
                      ada_flags)])

    str_source = to_strings(source)
    link_cmd = "gnatlink '{}' -o '{}'".format(
        "' '".join([main_source + '.ali'] + get_files_with_suffix(str_source, 'o')),
        str(target[0]))
    SideEffect(get_side_effect_files(str_source), target)
    return compile_cmd + [link_cmd]

def get_side_effect_files(source):
    side_effect_files = []
    for adb_file in get_files_with_suffix(source, 'adb'):
        basename = os.path.splitext(adb_file)[0]
        side_effect_files.append(basename + '.o')
        side_effect_files.append(basename + '.ali')
    return side_effect_files

def builder():
    return Builder(generator=_ada_commands_generator)
