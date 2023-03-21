/*
* Copyright (c) 2008-2012 Semantix and (c) 2012-2015 Neuropublic
*
* This file is part of the ASN1SCC tool.
*
* Licensed under the terms of GNU General Public Licence as published by
* the Free Software Foundation.
*
*  For more informations see License.txt file
*/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using System.IO;
using System.Text.RegularExpressions;
using System.Security.Cryptography;
using System.Drawing;

/*
 
<Settings>
  <run mode="single">
    <input>ASN.stg</input>
    <output>ASN.stg.fs</output>
    <modName>ASN</modName> 
  </run>
</Settings>
 
 
 */




//    let Define_subType_ia5string(td:FE_StringTypeDefinition) (prTd:FE_StringTypeDefinition) =
//    ST.lang<- CommonTypes.ProgrammingLanguage.Ada
//    ST.call "spec_a" "Define_subType_ia5string" [("t", td :> Object);("prTd", prTd :>Object)]


namespace parseStg2
{

    class Program
    {

        static bool hn(string name, string rule)
        {
            return name.StartsWith(rule) && name.Length > rule.Length && Char.IsUpper(name[rule.Length]);
        }
        
        static string detectTypeByParam(string name)
        {
            string[] knownStrings = { "p", "p1", "p2", "ptr", "i", "v", "v1", "v2", "c1", "c2" };
            if (knownStrings.Contains(name))
                return "string";
            if (name.StartsWith("arr_"))
                return string.Format("seq<{0}>", name.Replace("arr_", ""));

            if (hn(name, "arrs"))
                return "seq<string>";
            if (hn(name, "arrn"))
                return "seq<BigInteger>";
            if (hn(name, "arru"))
                return "seq<byte>";
            if (hn(name, "arro"))
                return "???array<###Object>";
            if (hn(name, "so"))
                return "string option";
            if (hn(name, "b"))
                return "bool";
            if (hn(name, "s"))
                return "string";
            if (hn(name, "no"))
                return "BigInteger option";
            if (hn(name, "n"))
                return "BigInteger";
            if (hn(name, "d"))
                return "double";
            if (hn(name, "c"))
                return "char";

            return "Object";
        }
        
        class Parameter
        {
            public string name { get; set; }
            public string type { get; set; }

            public static Parameter create(string l)
            {
                var name1 = l;
                var type1 = detectTypeByParam(l);

                if (l.Contains("/*:"))
                {
                    var tmp = l.Replace("/*", "").Replace("*/", "");
                    name1 = tmp.Split(':')[0];
                    type1 = tmp.Split(':')[1];
                }
                return new Parameter { name = name1, type = type1 };
            }

            public override string ToString()
            {
                return name;
            }

        }

        class Function
        {
            public string name { get; set; }
            public IEnumerable<Parameter> prms { get; set; }
            public string name2 { get { return name.Replace("_decode", "").Replace("_encode", ""); } }
            public string ID { get { return name2 + prms.Select(z => z.name).Join(""); } }

            public static IEnumerable<Function> readFromFile(String inpFileName)
            {
                var functions =
                        from line in skipComments(File.ReadAllLines(inpFileName).Skip(1))
                        where line.Contains("::=") && !line.Contains("/*nogen*/") && !line.Contains("DEFINITIONS AUTOMATIC TAGS")
                        let declPart = line.Split(new[] { "::=" }, StringSplitOptions.None)[0].Trim()
                        let Name = declPart.Split('(')[0].Trim()
                        let Prms = declPart.Split('(')[1].Split(')')[0].Split(',').Select(s => s.Trim()).Where(s => s != "").Select(l => Parameter.create(l))
                        select new Function { name = Name, prms = Prms };
                return functions;
            }
        }



        static int Main(string[] args)
        {
            var runMode = args.Length >= 2 ? int.Parse(args[1]) : 1;
            var outFile = args.Length >= 3 ? args[2] : "-";
            Console.WriteLine("Caling parseStg2 with args = " + args[0] + ", runMode: " + runMode + ", singleOutFile:" + outFile);

            var curDir = Path.GetDirectoryName(args[0]);
            XDocument doc = XDocument.Load(args[0]);

            int ret = 0;
            if (runMode == 4)
            {
                emitSingleFile(doc, curDir, outFile);
            }
            else 
                foreach (var elem in doc.Descendants("run"))
                {
                    if (elem.Attribute("mode").Value == "single")
                    {
                        if (runMode == 1) {
                            ret += ProcessSingle(elem, curDir);
                        } else if (runMode == 2) {
                            ret += emitAbstractInterface(elem, curDir);
                        } else if (runMode == 3)
                        {
                            ret += ProcessSingle(elem, curDir);
                            ret += emitInterfaceImplementation(elem, curDir);
                        }
                    }
                    
                }
            return ret;
        }


        static string MapParamName(string p)
        {
            //(p => "(\"" + p + "\"," + (p.StartsWith("arr")?p+"|>Seq.toArray":p) + " :>Object)")
            if (p.StartsWith("arrs"))
                return "(\"" + p + "\",(" + (p + "|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray") + ") :>Object)";
            if (p.StartsWith("so"))
                return "(\"" + p + "\"," + ("(if " + p + ".IsNone then null else ST.StrHelper " + p + ".Value:>Object") + ") )";
            if (p.StartsWith("s"))
                return "(\"" + p + "\"," + ("(if " + p + " = null then null else ST.StrHelper " + p + ":>Object") + ") )";
            if (p.StartsWith("no"))
                return "(\"" + p + "\"," + ("(if " + p + ".IsNone then null else " + p + ".Value:>Object") + ") )";
            return "(\"" + p + "\"," + (p.StartsWith("arr") ? p + "|>Seq.toArray" : p) + " :>Object)";
        }
        
        static IEnumerable<string> skipComments(IEnumerable<string> lines)
        {
            bool inComment = false;
            foreach (string l in lines)
            {
                if (inComment)
                {
                    if (l.Contains("*/"))
                    {
                        inComment = false;
                        yield return l.Split(new string[] { "*/" }, StringSplitOptions.None)[1];
                    }
                    else
                        continue;
                }

                if (l.Contains("/*nogen*/"))
                    yield return l;

                else if (Regex.IsMatch(l, "/\\*.*\\*/"))
                {
                    if (l.Contains("/*:"))
                        yield return l;
                    else
                        yield return Regex.Replace(l, "/\\*.*\\*/", "");
                }
                else if (l.Contains("/*"))
                {
                    inComment = true;
                    yield return l.Split(new string[] { "/*" }, StringSplitOptions.None)[1];
                }
                else
                    yield return l;
            }
        }
  
        static int ProcessSingle(XElement run, string curDir)
        {
            var inpFileName = Path.Combine(curDir, run.Element("input").Value);
            var outFileName = Path.Combine(curDir, run.Element("output").Value);
            var modName = run.Element("modName").Value;
            var UseAttr = run.Attribute("uses");
            var Uses = UseAttr == null ? new List<string>() { "CommonTypes" } : new List<string>(UseAttr.Value.Split(';').Where(c => c.Trim() != ""));
            string lng = "";
            switch (run.Element("lang").Value.ToUpper())
            {
                case "SPARK":
                    lng = "ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- false";
                    break;
                case "C":
                    lng = "ST.lang <- CommonTypes.ProgrammingLanguage.C; ST.double2StringPlain <- false";
                    break;
                case "SCALA":
                    lng = "ST.lang <- CommonTypes.ProgrammingLanguage.Scala; ST.double2StringPlain <- false";
                    break;
                case "ADA":
                    lng = "ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- false";
                    break;
                case "HTML":
                    lng = "ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true";
                    break;
                default:
                    lng = "ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true";
                    break;
            }

            var inpFileNoExt = Path.GetFileNameWithoutExtension(inpFileName);

            if (!File.Exists(inpFileName))
            {
                Console.Error.WriteLine("Input file {0} does not exist", inpFileName);
                return 1;
            }
            /*
            if (File.Exists(outFileName) && File.GetLastWriteTimeUtc(outFileName) > File.GetLastWriteTimeUtc(inpFileName))
            {
                //Console.WriteLine("Processing of file '{0}' skiped", inpFileName);
                return 0;
            }*/
            //Console.WriteLine("Generating file '{0}' 1", outFileName);

            var functions = Function.readFromFile(inpFileName);



            using (var txt = new StringWriter())
            {
                txt.WriteLine("module {0}", modName);
                txt.WriteLine("open System");
                txt.WriteLine("open System.Numerics");
                foreach(var md in Uses)
                    txt.WriteLine("open {0}", md);

                txt.WriteLine();
                foreach (var groupedFunc in functions.GroupBy(f => f.ID))
                {
                    foreach (var func in groupedFunc.Take(1))
                    {
                        
                        //var prms = func.prms.Select(p => "(\"" + p + "\"," + (p.StartsWith("arr") ? p + "|>Seq.toArray" : p) + " :>Object)").Join(";");
                        var prms = func.prms.Select(p => MapParamName(p.name)).Join(";");
                        var paramerters =
                                func.prms.Count() > 0 ?
                                func.prms.Select(p => "(" + p.name + ":" + p.type + ")").Join(" ") :
                                "()";
                        if (groupedFunc.Count() == 1)
                        {
                            if (inpFileNoExt == "generic")
                            {
                                txt.WriteLine("let {0} {1} (fileName:string) =", func.name, paramerters);
                                txt.WriteLine("    {0}", lng);
                                txt.WriteLine("    ST.call_generic {0} \"{1}\" [{2}]", "fileName", func.name, prms);
                            }
                            else if (inpFileNoExt == "icd_uper" || inpFileNoExt == "icd_acn" || inpFileNoExt == "icdtemplate_acn" || inpFileNoExt == "icdtemplate_uper")
                            {
                                txt.WriteLine("let {0} (fileName:string) {1}  =", func.name, paramerters);
                                txt.WriteLine("    {0}", lng);
                                txt.WriteLine("    ST.call_generic {0} \"{1}\" [{2}]", "fileName", func.name, prms);
                            }
                            else
                            {
                                txt.WriteLine("let {0} {1} =", func.name, paramerters);
                                txt.WriteLine("    {0}", lng);
                                txt.WriteLine("    ST.call \"{0}\" \"{1}\" [{2}]", inpFileNoExt, func.name, prms);
                            }
                        }
                        else
                        {
                            txt.WriteLine("let {0} {1} codec =", func.name2, paramerters);
                            txt.WriteLine("    {0}", lng);
                            txt.WriteLine("    match codec with");
                            txt.WriteLine("    | Encode    ->");
                            txt.WriteLine("        ST.call \"{0}\" \"{1}\" [{2}]", inpFileNoExt, func.name2 + "_encode", prms);
                            txt.WriteLine("    | Decode    ->");
                            txt.WriteLine("        ST.call \"{0}\" \"{1}\" [{2}]", inpFileNoExt, func.name2 + "_decode", prms);
                        }
                        txt.WriteLine();
                    }
                }
                writeFileIfNeeded(outFileName, txt.ToString());
            }
            return 0;
        }


        static String emitAbstractClass(string curDir, XElement run)
        {
            var inpFileName = Path.Combine(curDir, run.Element("input").Value);

            var functions = Function.readFromFile(inpFileName);
            var abstInterf = run.Element("abctractInterface");
            var absClassName = abstInterf.Attribute("modMame").Value;

            using (var txt = new StringWriter())
            {

                txt.WriteLine();
                txt.WriteLine("    [<AbstractClass>]");
                txt.WriteLine("    type {0} () =", absClassName);

                foreach (var groupedFunc in functions.GroupBy(f => f.ID))
                {
                    foreach (var func in groupedFunc.Take(1))
                    {
                        //var prms = func.prms.Select(p => MapParamName(p)).Join(";");
                        var paramerters =
                                func.prms.Count() > 0 ?
                                func.prms.Select(p => p.name + ":" + p.type).Join(" -> ") :
                                "unit";


                        if (groupedFunc.Count() == 1)
                        {
                            txt.WriteLine("        abstract member {0} : {1} -> string;", func.name2, paramerters);
                        }
                        else
                        {
                            txt.WriteLine("        abstract member {0} : {1} -> codec:Codec -> string;", func.name2, paramerters);
                        }
                    }
                }

                return txt.ToString();
            }


        }

        static int emitSingleFile(XDocument doc,string curDir, string outPath)
        {
            var abstractClassesf =
                doc.Descendants("run").
                Where(run => File.Exists(Path.Combine(curDir, run.Element("input").Value)) &&
                             run.Element("abctractInterface") != null).
                Select(run => emitAbstractClass(curDir, run));

            //var outPath = @"C:\prj\GitHub\asn1scc\CommonTypes\AbstractMacros.fs";
            using (var txt = new StringWriter())
            {

                txt.WriteLine(@"
module AbstractMacros
open System
open System.Numerics
open CommonTypes
(*
WARNING !!!
Automatically generated file.

Generated by the C stg macros with the following command
../../parseStg2/bin/Debug/net5.0/parseStg2.exe backends.xml 4
*)
                ");
                foreach(var k in abstractClassesf)
                {
                    txt.WriteLine(k);
                }
                writeFileIfNeeded(outPath, txt.ToString());
            }
            return 1;
        }


        static int emitAbstractInterface(XElement run, string curDir)
        {
            var abstInterf = run.Element("abctractInterface");
            
            if (abstInterf == null)
                return 0;
            var inpFileName = Path.Combine(curDir, run.Element("input").Value);
            var outFileName = Path.Combine(curDir, abstInterf.Value);

            if (!File.Exists(inpFileName))
            {
                Console.Error.WriteLine("Input file {0} does not exist", inpFileName);
                return 1;
            }
            /*
            if (File.Exists(outFileName) && File.GetLastWriteTimeUtc(outFileName) > File.GetLastWriteTimeUtc(inpFileName))
            {
                //Console.WriteLine("Processing of file '{0}' skiped", inpFileName);
                return 0;
            }
            Console.WriteLine("Generating file '{0}' 2", outFileName);
            */
            var modName = abstInterf.Attribute("modMame").Value;
            var UseAttr = run.Attribute("uses");
            var Uses = UseAttr == null ? new List<string>() { "CommonTypes" } : new List<string>(UseAttr.Value.Split(';').Where(c => c.Trim() != ""));

            var functions = Function.readFromFile(inpFileName);

            using (var txt = new StringWriter())
            {
                txt.WriteLine("module {0}", modName);
                txt.WriteLine("open System");
                txt.WriteLine("open System.Numerics");
                foreach (var md in Uses)
                    txt.WriteLine("open {0}", md);

                txt.WriteLine();
                txt.WriteLine("    [<AbstractClass>]");
                txt.WriteLine("    type AbsMacros () =");

                foreach (var groupedFunc in functions.GroupBy(f => f.ID)) {
                    foreach (var func in groupedFunc.Take(1)) {
                        //var prms = func.prms.Select(p => MapParamName(p)).Join(";");
                        var paramerters =
                                func.prms.Count() > 0 ?
                                func.prms.Select(p => p.name + ":" + detectTypeByParam(p.name) ).Join(" -> ") :
                                "unit";

                        if (groupedFunc.Count() == 1)  {
                            txt.WriteLine("        abstract member {0} : {1} -> string;", func.name2, paramerters);
                        }
                        else {
                            txt.WriteLine("        abstract member {0} : {1} codec -> string;", func.name2, paramerters);
                        }
                    }
                }
                writeFileIfNeeded(outFileName, txt.ToString());

            }
            return 0;

        }



        static int emitInterfaceImplementation(XElement run, string curDir)
        {
            var interfaceImpl = run.Element("implementationClass");
            var lmodName = run.Element("modName").Value;

            if (interfaceImpl == null)
                return 0;
            var inpFileName = Path.Combine(curDir, run.Element("input").Value);
            var outFileName = Path.Combine(curDir, interfaceImpl.Value);

            if (!File.Exists(inpFileName))
            {
                Console.Error.WriteLine("Input file {0} does not exist", inpFileName);
                return 1;
            }
            /*
            if (File.Exists(outFileName) && File.GetLastWriteTimeUtc(outFileName) > File.GetLastWriteTimeUtc(inpFileName))
            {
                //Console.WriteLine("Processing of file '{0}' skiped", inpFileName);
                return 0;
            }
            Console.WriteLine("Generating file '{0}' 3", outFileName);
            */
            var modName = interfaceImpl.Attribute("modMame").Value;
            var className = modName;
            var ifModName = interfaceImpl.Attribute("ifModMame").Value;

            var UseAttr = run.Attribute("uses");
            var Uses = UseAttr == null ? new List<string>() { "CommonTypes" } : new List<string>(UseAttr.Value.Split(';').Where(c => c.Trim() != ""));

            var functions = Function.readFromFile(inpFileName);

            using (var txt = new StringWriter())
            {
                txt.WriteLine("module {0}", modName);
                txt.WriteLine("open System");
                txt.WriteLine("open System.Numerics");
                foreach (var md in Uses)
                    txt.WriteLine("open {0}", md);

                txt.WriteLine();
                txt.WriteLine("type {0}() =", className);
                txt.WriteLine("    inherit {0}()", ifModName);

                foreach (var groupedFunc in functions.GroupBy(f => f.ID))
                {
                    foreach (var func in groupedFunc.Take(1))
                    {
                        //var prms = func.prms.Select(p => MapParamName(p)).Join(";");

                        var paramerters =
                                func.prms.Count() > 0 ?
                                func.prms.Select(p => "(" + p.name + ":" + p.type + ")").Join(" ") :
                                "()";

                        var args =
                                func.prms.Count() > 0 ?
                                func.prms.Select(p => p ).Join(" ") :
                                "()";

                        if (groupedFunc.Count() == 1)
                        {
                            txt.WriteLine("        override this.{0}  {1} =", func.name2, paramerters);
                            txt.WriteLine("            {0}.{1}  {2} ", lmodName, func.name2, args);
                        }
                        else
                        {
                            txt.WriteLine("        override this.{0}  {1} (codec:Codec) =", func.name2, paramerters);
                            txt.WriteLine("            {0}.{1}  {2} codec", lmodName, func.name2, args);
                        }
                    }
                }
                writeFileIfNeeded(outFileName, txt.ToString());
            }
            return 0;
        }

        public static string getHash(string input)
        {
            using(var md5Hash = MD5.Create())
            {
                var data = md5Hash.ComputeHash(Encoding.UTF8.GetBytes(input));
                return Convert.ToHexString(data);
            }
        }

        public static void writeFileWithHash(string path, string content0)
        {
            var content1 = (content0 + "\n").Replace("\r", "");
            var hash = getHash(content1);
            var content2 = "//" + hash + "\n" + content1;   // "//%s\n%s" hash content
            File.WriteAllText(path, content2.Replace("\r", ""), new System.Text.UTF8Encoding());

        }

        public static void writeFileIfNeeded(string path, string content)
        {
            content = content.Replace("\r", "");
            if (File.Exists(path)) {
                var existingContent = File.ReadAllText(path, new System.Text.UTF8Encoding());
                var existinMd5Hash = getHash(existingContent);
                var newContentHash = getHash(content);
                if (existinMd5Hash != newContentHash) {
                    Console.WriteLine("Generating file '{0}' 4", path);
                    File.WriteAllText(path, content, new System.Text.UTF8Encoding());
                } else {
                    Console.WriteLine("Info file '{0}' didn't regenated because file contents match", path);
                }
            } else {
                Console.WriteLine("Generating file '{0}' 5", path);
                File.WriteAllText(path, content, new System.Text.UTF8Encoding());
            }
        }


        /*
    let writeAllTextIfNotModified(path, content:string) =
    let writeFileWithHash(path, content:string) =
        let content = (content + "\n").Replace("\r", "")
        let hash = MD5.getHash content
        let content = sprintf "//%s\n%s" hash content
        File.WriteAllText(path, content.Replace("\r",""), new System.Text.UTF8Encoding())

    let dir = Path.GetDirectoryName path
    createDirectoryIfNotExists dir
    match File.Exists path with
    | true  -> 
        let existingContent = File.ReadAllText(path, new System.Text.UTF8Encoding())
        let firstLineBreak = existingContent.IndexOf('\n')
        if firstLineBreak > -1 then
            let line1 = existingContent.Substring(0,firstLineBreak)
            let restContent = existingContent.Substring(firstLineBreak + 1)
            if line1.Length >2 && (MD5.getHash restContent) = (line1.Substring(2).Trim()) then
                writeFileWithHash(path, content)

    | false -> writeFileWithHash(path, content)


*/


    }


    public static class MyExt
    {
        public static string Join<T>(this IEnumerable<T> pThis, string joiner)
        {
            if (pThis.Any())
                return pThis.Select(x => x.ToString()).Aggregate((cur, next) => cur + joiner + next);
            return "";
        }
    }

}
