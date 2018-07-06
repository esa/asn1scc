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
            var curDir = Path.GetDirectoryName(args[0]);
            XDocument doc = XDocument.Load(args[0]);
            var runMode = args.Length >= 2 ? int.Parse(args[1]) : 1;

            int ret = 0;
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
                    lng = "ST.lang <- CommonTypes.ProgrammingLanguage.Spark";
                    break;
                case "C":
                    lng = "ST.lang <- CommonTypes.ProgrammingLanguage.C";
                    break;
                case "ADA":
                    lng = "ST.lang <- CommonTypes.ProgrammingLanguage.Ada";
                    break;
                case "HTML":
                    lng = "ST.lang <- CommonTypes.ProgrammingLanguage.Html";
                    break;
                default:
                    lng = "ST.lang <- CommonTypes.ProgrammingLanguage.Unknown";
                    break;
            }

            var inpFileNoExt = Path.GetFileNameWithoutExtension(inpFileName);

            if (!File.Exists(inpFileName))
            {
                Console.Error.WriteLine("Input file {0} does not exist", inpFileName);
                return 1;
            }

            if (File.Exists(outFileName) && File.GetLastWriteTimeUtc(outFileName) > File.GetLastWriteTimeUtc(inpFileName))
            {
                Console.WriteLine("Processing of file '{0}' skiped", inpFileName);
                return 0;
            }

            var functions = Function.readFromFile(inpFileName);



            using (var txt = new StreamWriter(outFileName))
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
            }
            return 0;
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

            if (File.Exists(outFileName) && File.GetLastWriteTimeUtc(outFileName) > File.GetLastWriteTimeUtc(inpFileName))
            {
                Console.WriteLine("Processing of file '{0}' skiped", inpFileName);
                return 0;
            }

            var modName = abstInterf.Attribute("modMame").Value;
            var UseAttr = run.Attribute("uses");
            var Uses = UseAttr == null ? new List<string>() { "CommonTypes" } : new List<string>(UseAttr.Value.Split(';').Where(c => c.Trim() != ""));

            var functions = Function.readFromFile(inpFileName);

            using (var txt = new StreamWriter(outFileName))
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

            if (File.Exists(outFileName) && File.GetLastWriteTimeUtc(outFileName) > File.GetLastWriteTimeUtc(inpFileName))
            {
                Console.WriteLine("Processing of file '{0}' skiped", inpFileName);
                return 0;
            }

            var modName = interfaceImpl.Attribute("modMame").Value;
            var ifModName = interfaceImpl.Attribute("ifModMame").Value;

            var UseAttr = run.Attribute("uses");
            var Uses = UseAttr == null ? new List<string>() { "CommonTypes" } : new List<string>(UseAttr.Value.Split(';').Where(c => c.Trim() != ""));

            var functions = Function.readFromFile(inpFileName);

            using (var txt = new StreamWriter(outFileName))
            {
                txt.WriteLine("module {0}", modName);
                txt.WriteLine("open System");
                txt.WriteLine("open System.Numerics");
                foreach (var md in Uses)
                    txt.WriteLine("open {0}", md);

                txt.WriteLine();
                txt.WriteLine("let macrosImplClass =");
                txt.WriteLine("    {{new {0}.AbsMacros() with", ifModName);

                foreach (var groupedFunc in functions.GroupBy(f => f.ID))
                {
                    foreach (var func in groupedFunc.Take(1))
                    {
                        //var prms = func.prms.Select(p => MapParamName(p)).Join(";");
                        var paramerters =
                                func.prms.Count() > 0 ?
                                func.prms.Select(p => "(" + p + ":" + detectTypeByParam(p.name) +")").Join(" ") :
                                "()";
                        var args =
                                func.prms.Count() > 0 ?
                                func.prms.Select(p => p ).Join(" ") :
                                "()";

                        if (groupedFunc.Count() == 1)
                        {
                            txt.WriteLine("        member this.{0}  {1} =", func.name2, paramerters);
                            txt.WriteLine("            {0}.{1}  {2} ", lmodName, func.name2, args);
                        }
                        else
                        {
                            txt.WriteLine("        member this.{0}  {1} codec =", func.name2, paramerters);
                            txt.WriteLine("            {0}.{1}  {2} codec", lmodName, func.name2, args);
                        }
                    }
                }

                txt.WriteLine("    }");


            }
            return 0;
        }
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
