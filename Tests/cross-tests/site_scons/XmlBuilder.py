from SCons.Script import *

import commons

def _xml_generator(target, source, env, for_signature):
    return ["'{}' -ast '{}' '{}'".format(
                env['ASN_BIN'],
                str(target[0]),
                commons.get_files_with_suffix(source, '.asn1')[0])]

def builder():
    return Builder(generator=_xml_generator)
