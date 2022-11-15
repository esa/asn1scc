templates = [
R'''
#ifndef {test_case}_C_PROXY
#define {test_case}_C_PROXY
#include <stdbool.h>

static const char *filename = "{buffer_}";
bool proxy_decode();
void proxy_encode();
#endif
''',
R'''
#include "c_proxy.h"

#include <stdbool.h>
#include <stdio.h>
#include "../../file_utility.h"
#include "../{asn_header}"

bool proxy_decode() {{
  unsigned char buffer[{type_}_REQUIRED_BYTES_FOR_ACN_ENCODING];
  BitStream stream;
  BitStream_Init(&stream, buffer, {type_}_REQUIRED_BYTES_FOR_ACN_ENCODING);
  read_from_file(filename, (char *) buffer, {type_}_REQUIRED_BYTES_FOR_ACN_ENCODING);
  int error_code=0;
  {type_} to_decode;

  flag result={type_}_ACN_Decode(&to_decode, &stream, &error_code);
  if (result==false) {{
    fprintf(stderr,"Result of {type_}_ACN_Encode evaluated to false\n");
    return false;
  }}
  if (error_code!=0) {{
    fprintf(stderr,"Error code was set: %d\n",error_code);
    return false;
  }}
  if (remove(filename)!=0) {{
    fprintf(stderr,"Failed to remove file: %s\n",filename);
    perror(NULL);
    return false;
  }}
  printf("%llu\n",to_decode);
  fflush(stdout);
  return true;
}}

void proxy_encode(){{
  unsigned char buffer[{type_}_REQUIRED_BYTES_FOR_ACN_ENCODING];
  BitStream stream;
  BitStream_Init(&stream,buffer,{type_}_REQUIRED_BYTES_FOR_ACN_ENCODING);

  int error_code=0;
  flag result={type_}_ACN_Encode(&{test_case},&stream,&error_code,false);

  if (result==false) {{
    fprintf(stderr,"Result of {type_}_ACN_Encode evaluated to false\n");
    return;
  }}
  if (error_code!=0) {{
    fprintf(stderr,"Error code was set: %d\n",error_code);
    return;
  }}

  write_to_file(filename,(char *)buffer,{type_}_REQUIRED_BYTES_FOR_ACN_ENCODING);

  printf("%llu\n",{test_case});
  fflush(stdout);
}}
''',
R'''
with {module};
with Interfaces;
with Interfaces.C;

package ada_accessors is

function get(buffer : in {module}.{type_}_ACN_bit_array; index : in Interfaces.C.Int) return Interfaces.C.int;
procedure set(buffer : in out {module}.{type_}_ACN_bit_array; index : in Interfaces.C.Int; value : in Interfaces.C.int);

pragma Export(C, get, "get");
pragma Export(C, set, "set");

end ada_accessors;
''',
R'''
with {module};
with Interfaces;
with Interfaces.C;
with adaasn1rtl; use adaasn1rtl;
with Interfaces.C; use Interfaces.C;

package body ada_accessors is

function get(buffer : in {module}.{type_}_ACN_bit_array; index : in Interfaces.C.Int) return Interfaces.C.int is
    pragma Suppress(Index_Check);
    pragma Suppress(Overflow_Check);
begin
  if (buffer(Integer(index+1)) = 1) then
    return Interfaces.C.int(1);
  else
    return Interfaces.C.int(0);
  end if;
end get;

procedure set(buffer : in out {module}.{type_}_ACN_bit_array; index : in Interfaces.C.Int; value : in Interfaces.C.int) is
    pragma Suppress(Index_Check);
    pragma Suppress(Range_Check);
    pragma Suppress(Overflow_Check);
begin
  buffer(Integer(index+1)) := adaasn1rtl.BIT(value);
end set;

end ada_accessors;
''',
R'''
with {module};
with Interfaces;
with Interfaces.C;

package ada_proxy is

procedure proxy_encode;
procedure proxy_decode;
procedure save_to_file(buffer : in {module}.{type_}_ACN_bit_array);
procedure read_from_file(buffer : in out {module}.{type_}_ACN_bit_array);

function write_memory(filename : in String; buffer : in {module}.{type_}_ACN_bit_array) return Interfaces.C.int;
function read_memory(filename : in String; buffer : in out {module}.{type_}_ACN_bit_array) return Interfaces.C.int;

pragma Import(C, write_memory, "write_memory");
pragma Import(C, read_memory, "read_memory");

private
  filename : constant String := "{buffer_}";

end ada_proxy;
''',
R'''
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;
with Interfaces;
with adaasn1rtl; use adaasn1rtl;
with Interfaces.C; use Interfaces.C;
with Ada.Directories; use Ada.Directories;

package body ada_proxy is

procedure proxy_encode is
  stream : {module}.{type_}_ACN_Stream;
begin
  {module}.{type_}_ACN_Encode({module}.{test_case}, stream);
  save_to_file(stream.Data);
  Ada.Long_Long_Integer_Text_IO.Put(Long_Long_Integer({module}.{test_case}));
end proxy_encode;

procedure proxy_decode is
  stream : {module}.{type_}_ACN_Stream;
  value : {module}.{type_}:= 5;
  result : adaasn1rtl.ASN1_RESULT;
begin
  read_from_file(stream.Data);
  stream.DataLen := {module}.{type_}_REQUIRED_BITS_FOR_ACN_ENCODING;
  {module}.{type_}_ACN_Decode(value, stream, result);
  if (result.Success /= True) then
    Ada.Text_IO.Put(Ada.Text_IO.Standard_Error, "Error was set. Error code: ");
    Ada.Integer_Text_IO.Put(Ada.Text_IO.Standard_Error, result.ErrorCode);
    Ada.Text_IO.New_Line(Ada.Text_IO.Standard_Error);
  else
    Delete_File(filename);
    Ada.Long_Long_Integer_Text_IO.Put(Long_Long_Integer(value));
  end if;
  exception
    when Name_Error =>
        Ada.Text_IO.Put(Ada.Text_IO.Standard_Error,
            "Error when trying to delete a file, no such file or directory: ");
        Ada.Text_IO.Put(Ada.Text_IO.Standard_Error, filename);
        Ada.Text_IO.New_Line(Ada.Text_IO.Standard_Error);
    when Use_Error =>
        Ada.Text_IO.Put(Ada.Text_IO.Standard_Error,
            "Error when trying to delete a file, operation not permitted: ");
        Ada.Text_IO.Put(Ada.Text_IO.Standard_Error, filename);
        Ada.Text_IO.New_Line(Ada.Text_IO.Standard_Error);
end proxy_decode;

procedure save_to_file(buffer : in {module}.{type_}_ACN_bit_array) is
  result : Interfaces.C.int;
  c_name : String(1..filename'length + 1);
begin
  c_name(1..filename'length) := filename;
  c_name(filename'length + 1) := Character'Val(0);
  result := write_memory(c_name, buffer);
  if (result /= 0) then
    Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error, "Writing to file failed");
  end if;
end save_to_file;

procedure read_from_file(buffer : in out {module}.{type_}_ACN_bit_array) is
  result : Interfaces.C.int;
  c_name : String(1..filename'length + 1);
begin
  c_name(1..filename'length) := filename;
  c_name(filename'length + 1) := Character'Val(0);
  result := read_memory(c_name, buffer);
  if (result /= 0) then
    Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error, "Reading from file failed");
  end if;
end read_from_file;

end ada_proxy;
''',
R'''
#include <stdio.h>
#include <string.h>

#include "../{asn_header}"
#include "../../file_utility.h"

extern int get(char buffer[], int index);
extern void set(char buffer[], int index, int value);

int read_memory(const char *filename, char buffer[]) {{
    unsigned char real_buffer[{type_}_REQUIRED_BYTES_FOR_ACN_ENCODING];
    memset(real_buffer, 0, {type_}_REQUIRED_BYTES_FOR_ACN_ENCODING);

    read_from_file(filename, (char *) real_buffer, {type_}_REQUIRED_BYTES_FOR_ACN_ENCODING);

    for (int i = 0; i < {type_}_REQUIRED_BYTES_FOR_ACN_ENCODING; ++i) {{
        for (int j = 0; j < 8; ++j) {{
            int to_write = (real_buffer[i] & (1 << (7-j))) != 0;
            set(buffer, j+i*8, to_write);
        }}
    }}
    return 0;
}}

int write_memory(const char *filename, char buffer[]) {{
    unsigned char real_buffer[{type_}_REQUIRED_BYTES_FOR_ACN_ENCODING];
    memset(real_buffer, 0, {type_}_REQUIRED_BYTES_FOR_ACN_ENCODING);
    for (int i = 0; i < {type_}_REQUIRED_BYTES_FOR_ACN_ENCODING; ++i) {{
        for (int j = 0; j < 8; ++j) {{
            unsigned char bit = get(buffer, j+i*8);
            real_buffer[i] |= (bit << (7-j));
        }}
    }}

    write_to_file(filename, (char *) real_buffer, {type_}_REQUIRED_BYTES_FOR_ACN_ENCODING);
    return 0;
}}
'''
]
