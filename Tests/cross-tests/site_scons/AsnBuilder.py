from SCons.Script import *

from commons import to_strings

def _asn_generator(target, source, env, for_signature):
    str_source = to_strings(source)
    return ["'{}' -{} -ACN '{}' '{}' -o '{}'".format(
                env['ASN_BIN'],
                language,
                str(source[1]),
                str(source[2]),
                os.path.dirname(str(target[0])))
            for language in env['LANGUAGES']]

def _append_output(target, source, env):
    c_basename, ada_basename = env['BASENAMES']
    dirname = env['DIRNAME']
    OUTPUT = ['real.c',
              'asn1crt.h',
              'acn.c',
              '{}.h'.format(c_basename),
              '{}.c'.format(c_basename),
              'asn1crt.c',
              'adaasn1rtl.ads',
              'adaasn1rtl.adb',
              '{}.adb'.format(ada_basename),
              '{}.ads'.format(ada_basename),
              'gnat.cfg',
              'IgnoredExaminerWarnings.wrn',
              'GPS_project.gpr',
              'runSpark.sh',
              'spark.idx']
    for file_ in OUTPUT:
        target.append(os.path.join(dirname, file_))
    return target, source

def builder():
    return Builder(generator=_asn_generator, emitter=_append_output)
