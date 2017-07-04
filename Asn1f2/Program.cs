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

using Antlr.Runtime;
using Antlr.Runtime.Tree;
using System.IO;
using Antlr.Asn1;
using Antlr.Acn;
using Antlr;

namespace Asn1f2
{
    public class Program
    {
        static int Main(string[] args)
        {
            if (System.Diagnostics.Debugger.IsAttached)
                return Main2(args);
            try
            {
                return Main2(args);
            }
            catch (FsUtils.BugErrorException ex)
            {
                Console.Error.WriteLine("*****************************************************************************");
                Console.Error.WriteLine("* A fatal error has occurred");
                Console.Error.WriteLine("* Unexpected internal compiler error");
                Console.Error.WriteLine("*");
                Console.Error.WriteLine("* ");
                Console.Error.WriteLine("* Please report bugs to: https://github.com/ttsiodras/asn1scc/issues");
                Console.Error.WriteLine("* Error is:");
                Console.Error.WriteLine("* {0}", ex.Data0);
                Console.Error.WriteLine("* ");
                Console.Error.WriteLine("* Call stack is:");
                Console.Error.WriteLine("* ");
                foreach (var l in ex.StackTrace.Split('\n'))
                    Console.Error.WriteLine(l.Replace("\r", ""));
                Console.Error.WriteLine("*****************************************************************************");

                return 3;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex.Message);
                return 3;
            }

        }
        static int Main2(string[] args)
        {
            try
            {
                return CheckSuccess(args);
            }
            catch (asn1Parser.SyntaxErrorException ex)
            {
                Console.Error.WriteLine(ex.Message);
                return 1;
            }
            catch (FsUtils.SemanticError ex)
            {
                if (ex.Data0.Equals(FsUtils.emptyLocation))
                    Console.Error.WriteLine("error: {0}", ex.Data1);
                else
                    Console.Error.WriteLine(ErrorFormatter.FormatError(ex.Data0.srcFilename, ex.Data0.srcLine, ex.Data0.charPos, ex.Data1));
                return 2;
            }
        }

        public static CommonTypes.EnumRenamePolicy getRenamePolicy(CmdLineArgs.CmdLineArguments cmdArgs, CommonTypes.EnumRenamePolicy defaultValue)
        {
            var renamePolicyStr = cmdArgs.GetOptionalArgument("renamePolicy", "xxx");
            var renamePolicy = defaultValue;
            if (renamePolicyStr == "0")
                renamePolicy = CommonTypes.EnumRenamePolicy.NoRenamePolicy;
            else if (renamePolicyStr == "1")
                renamePolicy = CommonTypes.EnumRenamePolicy.SelectiveEnumerants;
            else if (renamePolicyStr == "2")
                renamePolicy = CommonTypes.EnumRenamePolicy.AllEnumerants;
            else if (renamePolicyStr!="xxx")
            {
                Console.Error.WriteLine("Invalid value for argument 'renamePolicy'\nValid values are 0,1,2");
                Environment.Exit(Usage());
            }

            return renamePolicy;
        }


        public static int CheckSuccess(IEnumerable<string> args)
        {
            var aaa = Resource1.asn1crt;

            var asn1InputFiles = args.Where(a => !a.StartsWith("-") && (a.ToLower().EndsWith(".asn1") || a.ToLower().EndsWith(".asn")));
            var acnInputFiles = args.Where(a => !a.StartsWith("-") && a.ToLower().EndsWith(".acn"));
            
            foreach (var f in asn1InputFiles.Concat(acnInputFiles))
                if (!File.Exists(f))
                    throw new FsUtils.SemanticError(new FsUtils.SrcLoc(f, 0, 0), string.Format("File does not exist"));

            var cmdArgs = CmdLineArgs.CmdLineArguments.GetInstance(Usage);
            CmdLineArgs.CmdLineArguments.Args = args.ToArray();
            cmdArgs.RegisterArg(new List<CmdLineArgs.CmdArg>() 
            { 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "equal", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "c", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "Ada", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "uPER", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "ACN", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "ACND", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "XER", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "BER", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "o", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "wordSize", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "ast", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "customStg", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "customIcdUper", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "customIcdAcn", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "customStgAstVerion", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "customStgAstVersion", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "icdUper", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "icdAcn", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "vc", Madatory=false},
                new CmdLineArgs.CmdArg { HasValue = false, Name = "debug", Madatory=false},
                new CmdLineArgs.CmdArg { HasValue = false, Name = "bast", Madatory=false},
                new CmdLineArgs.CmdArg { HasValue = true, Name = "typePrefix", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "help", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "atc", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "oss", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = false, Name = "AdaUses", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "renamePolicy", Madatory=false}, 
                new CmdLineArgs.CmdArg { HasValue = true, Name = "mfm", Madatory=false}
            });
            cmdArgs.CheckArguments();
            if (cmdArgs.HasArgument("h"))
                Environment.Exit(Usage());
            if (!asn1InputFiles.Any())
            {
                Console.Error.WriteLine("No input files");
                Environment.Exit(Usage());
            }
            var generateEqualFunctions = cmdArgs.HasArgument("equal") || cmdArgs.HasArgument("atc");
            var outDir = cmdArgs.GetOptionalArgument("o") ?? ".";
            var bGenerateAcnDefault = cmdArgs.HasArgument("ACND");

            var ws = cmdArgs.GetOptionalArgument("wordSize", "8");
            if (ws != "8")
            {
                Console.Error.WriteLine("Invalid argument wordSize");
                return 4;
            }


            if (!Directory.Exists(outDir))
            {
                Console.Error.WriteLine("Output directory '{0}' does not exist", outDir);
                return 4;
            }

            bool bDebug = cmdArgs.HasArgument("debug");
            bool bast = cmdArgs.HasArgument("bast");
            List<CommonTypes.Asn1Encoding> encodings = new List<CommonTypes.Asn1Encoding>();
            if (cmdArgs.HasArgument("uPER"))
                encodings.Add(CommonTypes.Asn1Encoding.UPER);
            if (cmdArgs.HasArgument("ACN"))
                encodings.Add(CommonTypes.Asn1Encoding.ACN);
            if (cmdArgs.HasArgument("XER"))
                encodings.Add(CommonTypes.Asn1Encoding.XER);
            if (cmdArgs.HasArgument("BER"))
                encodings.Add(CommonTypes.Asn1Encoding.BER);

            var asn1Files = ParseAsn1InputFiles(asn1InputFiles);

            var astXmlFile = cmdArgs.GetOptionalArgument("ast", "");
            if (!String.IsNullOrEmpty(astXmlFile) && !(astXmlFile.ToLower().EndsWith(".xml")))
            {
                Console.Error.WriteLine("Invalid output filename '{0}'\nGenerated ast xml files must have an .xml extension.", astXmlFile);
                return 4;
            }



            var icdUperHtmlFileName = cmdArgs.GetOptionalArgument("icdUper", "");
            if (!String.IsNullOrEmpty(icdUperHtmlFileName) && !(icdUperHtmlFileName.ToLower().EndsWith(".html") || icdUperHtmlFileName.ToLower().EndsWith(".htm")))
            {
                Console.Error.WriteLine("Invalid output filename '{0}'\nGenerated icd files must have an .html or .htm extension.", icdUperHtmlFileName);
                return 4;
            }

            var icdAcnHtmlFileName = cmdArgs.GetOptionalArgument("icdAcn", "");
            if (!String.IsNullOrEmpty(icdAcnHtmlFileName) && !(icdAcnHtmlFileName.ToLower().EndsWith(".html") || icdAcnHtmlFileName.ToLower().EndsWith(".htm")))
            {
                Console.Error.WriteLine("Invalid output filename '{0}'\nGenerated icd files must have an .html or .htm extension.", icdAcnHtmlFileName);
                return 4;
            }

            var mappingFunctionsModule = cmdArgs.GetOptionalArgument("mfm", null);

            /*
             * constructs a parameterized (templatized) ASN.1 AST by antlr trees.
             * A parameterized ASN.1 AST is the one that contains generic types. E.g.
             * 	
             * FrequenciesTemplate{INTEGER:someLength, SomeType } ::= SEQUENCE (SIZE(someLength)) OF SomeType
             * 
		     * MyTestInt ::= FrequenciesTemplate{5, INTEGER(1..20)}
		     * MyTestReal ::= FrequenciesTemplate{5, REAL}
             * 
             * 
            */
            var parameterized_ast = CreateAsn1AstFromAntlrTree.CreateAstRoot(asn1Files, encodings.ToArray(),
                    generateEqualFunctions, cmdArgs.GetOptionalArgument("typePrefix", ""), cmdArgs.HasArgument("oss"),
                    astXmlFile, icdUperHtmlFileName, icdAcnHtmlFileName, mappingFunctionsModule, int.Parse(ws));

            /*
             *  Removes parameterized types by resolving them. In the example above
             *  FrequenciesTemplate will no longer exist
             *  and MyTestInt and MyTestReal will defined as follows
             *  MyTestInt ::= SEQUENCE (SIZE(5)) OF INTEGER(1..20)
             *  MyTestReal ::= SEQUENCE (SIZE(5)) OF REAL
             */
            var asn1Ast0 = MapParamAstToNonParamAst.DoWork(parameterized_ast);

            //PrintAsn1.DoWork(asn1Ast0, outDir, ".0.asn1");

            /*
             * Performs semantic validations which cannot be handled during ANTLR parsing.
             * For example the following definition
             * MySeq ::= {a INTEGER, a REAL}
             * is OK for ANTLR but of course not OK for ASN.1
             */
            CheckAsn1.CheckFiles(asn1Ast0, 0);

            if (astXmlFile != "")
            {
                var renamePolicy = getRenamePolicy(cmdArgs, CommonTypes.EnumRenamePolicy.SelectiveEnumerants);
                var uniqueEnums = EnsureUniqueEnumNames.DoWork(asn1Ast0, renamePolicy);
                //XmlAst.DoWork(uniqueEnums);

                genericBackend.DoWork(uniqueEnums, "xml.stg", uniqueEnums.AstXmlAbsFileName);

                return 0;
            }

            /*
             * Creates an ACN ast taking as INPUT ANTLR parse trees and the ASN1 AST.
             * 
             */
            var acnAstUnresolved = AcnCreateFromAntlr.CreateAcnAst(ParseAcnInputFiles(acnInputFiles), asn1Ast0);
            //PrintAsn1.DoWork(asn1Ast0, outDir, ".0.asn1");
            //PrintAsn1.AcnPrint.print_AcnAst(acnAstUnresolved, "a_acn.raw.txt");

            /*
             * The ASN.1 AST is enriched from ACN ast
             */
            /*
            if (bast)
            {
                //var bast0 = BAstConstruction.createValidationAst(Ast.ProgrammingLanguage.C, asn1Ast0, acnAstUnresolved);
                //print_debug.DoWork(bast0, outDir, ".txt");
                //CAst.mapBastToCast(bast0, acnAstUnresolved);

                if (cmdArgs.HasArgument("c"))
                {
                    WriteTextFile(Path.Combine(outDir, "asn1crt.c"), Resource1.asn1crt);
                    WriteTextFile(Path.Combine(outDir, "asn1crt.h"), Resource1.asn1crt1);
                    WriteTextFile(Path.Combine(outDir, "acn.c"), Resource1.Acn);
                    WriteTextFile(Path.Combine(outDir, "real.c"), Resource1.real);
                    if (encodings.Contains(CommonTypes.Asn1Encoding.BER))
                    {
                        WriteTextFile(Path.Combine(outDir, "ber.c"), Resource1.ber);
                    }
                    if (encodings.Contains(CommonTypes.Asn1Encoding.XER))
                    {
                        WriteTextFile(Path.Combine(outDir, "xer.c"), Resource1.xer);
                    }
                    var renamePolicy = getRenamePolicy(cmdArgs, ParameterizedAsn1Ast.EnumRenamePolicy.SelectiveEnumerants);
                    var uniqueEnums = EnsureUniqueEnumNames.DoWork(asn1Ast0, renamePolicy);
                    BackendAstConstruct.DoWork(Ast.ProgrammingLanguage.C, uniqueEnums, acnAstUnresolved, outDir);
                } else if (cmdArgs.HasArgument("Ada"))
                {
                    WriteTextFile(Path.Combine(outDir, spark_body.GetRTLName() + ".adb"), Resource1.adaasn1rtl_adb);
                    WriteTextFile(Path.Combine(outDir, spark_body.GetRTLName() + ".ads"), Resource1.adaasn1rtl_ads);
                    WriteTextFile(Path.Combine(outDir, "IgnoredExaminerWarnings.wrn"), Resource1.IgnoredExaminerWarnings);
                    WriteTextFile(Path.Combine(outDir, "gnat.cfg"), Resource1.gnat);
                    WriteTextFile(Path.Combine(outDir, "runSpark.sh"), Resource1.run);
                    WriteTextFile(Path.Combine(outDir, "GPS_project.gpr"), Resource1.GPS_project);
                    var renamePolicy = getRenamePolicy(cmdArgs, ParameterizedAsn1Ast.EnumRenamePolicy.NoRenamePolicy);
                    var uniqueEnums = EnsureUniqueEnumNames.DoWork(asn1Ast0, renamePolicy);
                    BackendAstConstruct.DoWork(Ast.ProgrammingLanguage.Ada, uniqueEnums, acnAstUnresolved, outDir);
                    System.IO.Directory.CreateDirectory(Path.Combine(outDir, "examiner"));
                    System.IO.Directory.CreateDirectory(Path.Combine(outDir, "bin"));
                }

                return 0;
            }*/


            /*
             * The ASN.1 AST is enriched with information from ACN. In particular:
             * - the Asn1Type.AcnProperties are populated (up to this point it was just an empty list)
             * - creates types inserted in ACN (ChildInfo.AcnInsertedField = true). 
             * - validates that acn properties that have a relative paths are valid (i.e. point to existing 
             *   and compatibe types.
             */
            var asn1Ast = UpdateAcnProperties.DoWork(asn1Ast0, acnAstUnresolved).Item1;


            /*
             * Creates the "resolved" version of the ACN AST i.e.   AcnTypes.AcnAst -> AcnTypes.AcnAstResolved
             * acn.Constants and acn.Parameters are copied as is from one AST to the other
             * References are convered from AcnTypes.LongReference (the ones containing relative paths) to LongReferenceResolved (only absolute paths)
             * By the way, LongReference and LongReferenceResolved are really bad names. They should be called RelativeLink and absolute links
             * since they link two types 
             *  -the decType (the ASN.1 type that is encoded/decode)
             *  -the determinant that acts as some kind of determinant (e.g. size determinant, presence determinant etc)
             *  -The kind of determinant is described by the property Kind : LongReferenceKind 
             *  At this point the TmpTypes is empty (TmpTypes are created during the Acn.RemoveVirtualPaths step)
             */
            var acnAstResolved = Acn.Resolve.ResolveRelativePaths(acnAstUnresolved, asn1Ast);
            //PrintAsn1.DebugPrintAsn1Acn(asn1Ast, acnAstResolved, ".", ".1.asn1");


            /*
             * Performs two main transformations
             * - NumericStrings are replaced with IA5Strings with the equivalent FROM ("0-9") constraint
             * - calculate values in enums that ware not provided with values in ASN.1
             */ 
            var asn1Ast_1 = RemoveNumericStringsAndFixEnums.DoWork(asn1Ast);

            /*
             * Some complex inner types in SEQUENCES, CHOICES etc are replace with new referenced types
             * E.g the followin ASN.1
             * MySeq :: = SEQUENCE { a INTEGER, innerSeq SEQUENCE {b REAL, c WHATEVER}} is transformed as follows
             * 
             * MySeq_innerSeq ::= SEQUENCE {b REAL, c WHATEVER}
             * MySeq :: = SEQUENCE { a INTEGER, innerSeq MySeq_innerSeq}
             */
            var noInnerasn1Ast = ReplaceInnerTypes.DoWork(asn1Ast_1, acnAstResolved, false);
            //PrintAsn1.DebugPrintAsn1Acn(noInnerasn1Ast.Item1, noInnerasn1Ast.Item2, ".", ".1b.asn1");

            /*
             * replace reference types that have constraints or ACN properties with a new reference type that has no constraints
             * e.g. The following 
             * MyInt ::= INTEGER (1..10)
             * MySeq ::= {a MyInt(2..5)}
             * 
             * is transformed as 
             * MyInt ::= INTEGER(1..10)
             * MyInt-1 ::= INTEGER(1..10)(2..5)
             * MySeq ::= {a MyInt-1}
            */
            var refTypesWithNoConstraints_asn1_acn = RemoveConstraintsFromRefTypes.DoWork(noInnerasn1Ast.Item1, noInnerasn1Ast.Item2);

            /*
             * Semantic validation
             */ 
            CheckAsn1.CheckFiles(refTypesWithNoConstraints_asn1_acn.Item1, 1);

            var refTypesWithNoConstraints = refTypesWithNoConstraints_asn1_acn.Item1;
            var acnAst2 = refTypesWithNoConstraints_asn1_acn.Item2;

            //PrintAsn1.DebugPrintAsn1Acn(refTypesWithNoConstraints, acnAst2, ".", ".2.asn1");

            /*
             *  Semantic validation
             */ 
            RemoveConstraintsFromRefTypes.CheckReferences(refTypesWithNoConstraints, acnAst2);

            /*
            What is a virtual path? The path x.y.z.q is virual if any of x,y or z is a referenced type.
            Take for instance the following ASN.1 and ACN grammars
            -- ASN1
	        INT10 ::= INTEGER(1..10)
	
	        TAP3File::=SEQUENCE {
                header  SEQUENCE {
			        operatorID  IA5String(SIZE(1..10))
		        },
                calls   SEQUENCE (SIZE(1..10)) OF INTEGER (1..20)
            }
            -- ACN
	        INT10[size 10, encoding pos-int]

            TAP3File[] {
                 header [] {
			        operatorID [],
			        nrCalls  INT10  [] 
		         },  
                 calls[size header.nrCalls]
            }
	
            In this grammar, we want the number elements in the calls SEQUENCE OF to be determined
            by a field in the header. This is accomplished by putting the size encoding property
              [size header.nrCalls]

            
            The path  'header.nrCalls' is not virtual since header is not a referenced type but a SEQUENCE type.
            However, due to the ReplaceInnerTypes.DoWork transformation the grammar has been transformed as follows

            --ASN.1
	        INT10 ::= INTEGER(1..10)
	        TAP3File-header ::= SEQUENCE {
			        operatorID  IA5String(SIZE(1..10))
		    }
	        
            TAP3File::=SEQUENCE {
                header  TAP3File-header,
                calls   SEQUENCE (SIZE(1..10)) OF INTEGER (1..20)
            }

            -- ACN
	        INT10[size 10, encoding pos-int]

            TAP3File-header [] {
			    operatorID [],
			    nrCalls  INT10  [] 
		    }  
            TAP3File[] {
                header  [],
                calls[size header.nrCalls]
            }
            Now the path 'header.nrCalls' IS virtual since header is a referenced type.
            The function Acn.RemoveVirtualPaths removes suchs paths. In this case the following trasnformation 
            will take place
            (1) The TAP3File will get a new TEMP type named nrCalls
            (2) size property changes from 
                [size header.nrCalls] to [size nrCalls] but now nrCalls
                is the TEMP type introduced in step1
            (2) The TAP3File-header type becomes parameterized and new parameter mode is EncodeDecode (i.e. used in both encoding/decoding)
                
                TAP3File-header <nrCalls0:INT10>[] {
			        operatorID [],
			        nrCalls  INT10  [] 
		        },  

            */
            var acnAst3 = Acn.RemoveVirtualPaths(refTypesWithNoConstraints, acnAst2);
            
            //last validation check, it may throw a SemanticException
            Acn.CheckForUnreferencedAcnInsertedFields(refTypesWithNoConstraints, acnAst3);
            
            if (!RemoveConstraintsFromRefTypes.CheckReferences(refTypesWithNoConstraints, acnAst3))
            {
                throw new FsUtils.BugErrorException("Broken paths");
            }
            if (bDebug) PrintAsn1.DebugPrintAsn1Acn(refTypesWithNoConstraints, acnAst3, outDir, ".3.asn1");

            var bGenTestCases = cmdArgs.HasArgument("atc");

            string[] astVersions = { "1", "2", "3", "4" };
            var customStgAstVer = cmdArgs.GetOptionalArgument("customStgAstVersion", cmdArgs.GetOptionalArgument("customStgAstVerion", "1"));
            if (!astVersions.Contains(customStgAstVer))
            {
                Console.Error.WriteLine("Invalid value of customStgAstVesrion argument.\nPlease provide one of the following values:");
                Console.Error.WriteLine("\t1\t==> Ast version where parameterized types have been removed");
                Console.Error.WriteLine("\t2\t==> Ast version where inner types have been removed");
                Console.Error.WriteLine("\t3\t==> Ast version where contraint reference types have been removed");
                Console.Error.WriteLine("\t4\t==> Ast version where Enumerated names are unique (required by C backend)");
                return 4;
            }

            var customStg = cmdArgs.GetOptionalArgument("customStg", "");
            if (customStg != "")
            {
                var astForCustomBackend = asn1Ast0;
                if (customStgAstVer == "1")
                    astForCustomBackend = asn1Ast0;
                else if (customStgAstVer == "2")
                    astForCustomBackend = noInnerasn1Ast.Item1;
                else if (customStgAstVer == "3")
                    astForCustomBackend = refTypesWithNoConstraints;
                else if (customStgAstVer == "4")
                {
                    var renamePolicy = getRenamePolicy(cmdArgs, CommonTypes.EnumRenamePolicy.SelectiveEnumerants);
                    astForCustomBackend = EnsureUniqueEnumNames.DoWork(refTypesWithNoConstraints, renamePolicy);
                }

                exportCustomStg(cmdArgs, customStg, "customStg", (stgFileName, outFileName) =>
                { 
                    genericBackend.DoWork(astForCustomBackend, stgFileName, outFileName); 
                });
            }

            var customIcdUper = cmdArgs.GetOptionalArgument("customIcdUper", "");
            if (customIcdUper != "")
            {
                exportCustomStg(cmdArgs, customIcdUper, "customIcdUper", (stgFileName, outFileName) =>
                {
                    var noInnerasn1Ast2 = ReplaceInnerTypes.DoWork(asn1Ast, acnAstResolved, true);
                    icdUper.DoWork(stgFileName, noInnerasn1Ast2.Item1, acnAst3, outFileName);
                });
            }

            var customIcdAcn = cmdArgs.GetOptionalArgument("customIcdAcn", "");
            if (customIcdAcn != "")
            {
                exportCustomStg(cmdArgs, customIcdAcn, "customIcdAcn", (stgFileName, outFileName) =>
                {
                    var noInnerasn1Ast2 = ReplaceInnerTypes.DoWork(asn1Ast, acnAstResolved, true);
                    icdAcn.DoWork(stgFileName, noInnerasn1Ast2.Item1, acnAstResolved, outFileName);
                });
            }



            if (cmdArgs.HasArgument("Ada"))
            {
                var renamePolicy = getRenamePolicy(cmdArgs, CommonTypes.EnumRenamePolicy.NoRenamePolicy);
                var astForBackend = EnsureUniqueEnumNames.DoWork(refTypesWithNoConstraints, renamePolicy);

                spark_body.DoWork(astForBackend, acnAst3, outDir);

                if (bGenTestCases)
                {
                    var vas = "ALL";

                    spark_automatic_test_cases.CreateTestCases(astForBackend, acnAst3, outDir);
                    spark_main.CreateMainFile(astForBackend, acnAst3, outDir, vas);
                    spark_main.CreateMakeFile(astForBackend, outDir);
                }

                WriteTextFile(Path.Combine(outDir, spark_body.GetRTLName() + ".adb"), Resource1.adaasn1rtl_adb);
                WriteTextFile(Path.Combine(outDir, spark_body.GetRTLName() + ".ads"), Resource1.adaasn1rtl_ads);
                WriteTextFile(Path.Combine(outDir, "IgnoredExaminerWarnings.wrn"), Resource1.IgnoredExaminerWarnings);
                WriteTextFile(Path.Combine(outDir, "gnat.cfg"), Resource1.gnat);
                WriteTextFile(Path.Combine(outDir, "runSpark.sh"), Resource1.run);
                WriteTextFile(Path.Combine(outDir, "GPS_project.gpr"), Resource1.GPS_project);

                spark_main.CreateSparkIndexFile(astForBackend, bGenTestCases, outDir);

                System.IO.Directory.CreateDirectory(Path.Combine(outDir, "examiner"));
                System.IO.Directory.CreateDirectory(Path.Combine(outDir, "bin"));
            }
            /*else */if (cmdArgs.HasArgument("c"))
            {
                WriteTextFile(Path.Combine(outDir, "asn1crt.c"), Resource1.asn1crt);
                WriteTextFile(Path.Combine(outDir, "asn1crt.h"), Resource1.asn1crt1);
                WriteTextFile(Path.Combine(outDir, "acn.c"), Resource1.Acn);
                WriteTextFile(Path.Combine(outDir, "real.c"), Resource1.real);
                if (encodings.Contains(CommonTypes.Asn1Encoding.BER)) {
                    WriteTextFile(Path.Combine(outDir, "ber.c"), Resource1.ber);
                }
                if (encodings.Contains(CommonTypes.Asn1Encoding.XER)) {
                    WriteTextFile(Path.Combine(outDir, "xer.c"), Resource1.xer);
                }

                var renamePolicy = getRenamePolicy(cmdArgs, CommonTypes.EnumRenamePolicy.SelectiveEnumerants);
                CheckAsn1.checkDuplicateValueAssigments(refTypesWithNoConstraints, CommonTypes.ProgrammingLanguage.C);
                var astForBackend = EnsureUniqueEnumNames.DoWork(refTypesWithNoConstraints, renamePolicy);
                c_body.DoWork(astForBackend, acnAst3, outDir);

                if (bGenTestCases)
                {
                    var vas = "ALL";

                    automatic_test_cases.CreateTestCases(astForBackend, acnAst3, outDir);
                    automatic_test_cases.CreateTestSuiteFile(astForBackend, acnAst3, outDir, vas);
                    automatic_test_cases.CreateMainFile(outDir);
                    automatic_test_cases.CreateMakeFile(astForBackend, acnAst3, outDir);
                }

            }
            /*else */if (bGenerateAcnDefault)
            {
                c_body.EmmitDefaultACNGrammar(asn1Ast0, outDir);
            }
            /*else */
            {
                if (cmdArgs.HasArgument("AdaUses"))
                    foreach (var s in Ast.AdaUses(refTypesWithNoConstraints))
                        Console.WriteLine(s);

                if (cmdArgs.HasArgument("icdUper"))
                {
                    var noInnerasn1Ast2 = ReplaceInnerTypes.DoWork(asn1Ast, acnAstResolved, true);
                    var htmlFileName = Path.Combine(outDir, noInnerasn1Ast2.Item1.IcdUperHtmlFileName);
                    icdUper.DoWork("icd_uper.stg", noInnerasn1Ast2.Item1, acnAst3, htmlFileName);
                }
                if (cmdArgs.HasArgument("icdAcn"))
                {

                    var renamePolicy = getRenamePolicy(cmdArgs, CommonTypes.EnumRenamePolicy.NoRenamePolicy);
                    var astForBackend = EnsureUniqueEnumNames.DoWork(refTypesWithNoConstraints, renamePolicy);

                    var htmlFileName = Path.Combine(outDir, astForBackend.IcdAcnHtmlFileName);
                    icdAcn.DoWork("icd_acn.stg", astForBackend, acnAst3, htmlFileName);
                    var cssFileName = Path.ChangeExtension(htmlFileName, ".css");
                    icdAcn.emitCss("icd_acn.stg", astForBackend, acnAst3, cssFileName);

                    /*
                    var noInnerasn1Ast2 = ReplaceInnerTypes.DoWork(asn1Ast, acnAstResolved, true);
                    var htmlFileName = Path.Combine(outDir, noInnerasn1Ast2.Item1.IcdAcnHtmlFileName);
                    icdAcn.DoWork("icd_acn.stg", noInnerasn1Ast2.Item1, acnAstResolved, htmlFileName);
                    var cssFileName = Path.ChangeExtension(htmlFileName, ".css");
                    icdAcn.emitCss("icd_acn.stg", noInnerasn1Ast2.Item1, acnAstResolved, cssFileName);
                    */
                }

            }

            return 0;
        }


        public static Tuple<ITree, string, IToken[]> Parse<L, P>(string filename, Func<ANTLRFileStream, L> lexer, Func<ITokenStream, P> parser, Func<P, ITree> root)
            where L : Lexer where P : Parser
        {
            var stream = new CommonTokenStream(lexer(new ANTLRFileStream(filename)));
            var tokens = stream.GetTokens().Cast<IToken>().ToArray();
            var tree = root(parser(stream));
            return Tuple.Create(tree, filename, tokens);
        }

        public static List<Tuple<ITree, string, IToken[]>> ParseAsn1InputFiles(IEnumerable<string> inputFiles)
        {
            return inputFiles.Select(f => Parse(f, fs => new asn1Lexer(fs), ts => new asn1Parser(ts), p => (CommonTree)p.moduleDefinitions().Tree)).ToList();
        }

        public static List<Tuple<ITree, string, IToken[]>> ParseAcnInputFiles(IEnumerable<string> inputFiles)
        {
            return inputFiles.Select(f => Parse(f, fs => new acnLexer(fs), ts => new acnParser(ts), p => (CommonTree)p.moduleDefinitions().Tree)).ToList();
        }


        static void exportCustomStg(CmdLineArgs.CmdLineArguments cmdArgs, String customStg, String cmdLingName, Action<string, string> backendInvocation)
        {
            var files = customStg.Split(':');
            if (files.Length != 2)
            {
                files = customStg.Split(new string[] { "::" }, StringSplitOptions.RemoveEmptyEntries);
                if (files.Length != 2)
                {
                    Console.Error.WriteLine("Invalid usage of {0} argument. Please use ':' to seperate the stg file with output file", cmdLingName);
                    Console.Error.WriteLine("E.g. -{0} mystg.stg:output.txt ", cmdLingName);
                    Console.Error.WriteLine("Under windows, you may user double :: to separate the stg file with output file");
                    Console.Error.WriteLine("E.g. -{0} c:\\mystg.stg::c:\\output.txt ", cmdLingName);
                    Environment.Exit(4);
                }
            }
            var stgFileName = files.First();
            var outFileName = files.Last();
            if (!File.Exists(stgFileName))
            {
                Console.Error.WriteLine("Custom stg file '{0}' does not exist", stgFileName);
                Environment.Exit(4);
            }


            backendInvocation(stgFileName, outFileName);
        }


        static int Usage()
        {
            var procName = "asn1";
            Console.Error.WriteLine();
            Console.Error.WriteLine("Semantix ASN.1 Compiler");
            Console.Error.WriteLine("Current Version is: 3.3.{0} ", Svn.Version);
            Console.Error.WriteLine("Usage:");
            Console.Error.WriteLine();
            Console.Error.WriteLine("{0}  <OPTIONS> file1, file2, ..., fileN ", procName);
            Console.Error.WriteLine();
            Console.Error.WriteLine("Where OPTIONS are:\n\n");
            Console.Error.WriteLine("\t -c\t\t\tgenerate code for the C/C++ programming language");
            Console.Error.WriteLine("\t -Ada\t\t\tgenerate code for the Ada/SPARK programming language");
            Console.Error.WriteLine("\t -uPER\t\t\tgenerates encoding and decoding functions for");
            Console.Error.WriteLine("\t\t\t\tunaligned Packed Encoding Rules (uPER)");
            Console.Error.WriteLine("\t -ACN\t\t\tgenerates encoding and decoding functions using");
            Console.Error.WriteLine("\t\t\t\tthe ASSERT ASN.1 encoding Control Notation");
            Console.Error.WriteLine("\t -ACND\t\t\tcreates ACN grammars for the input ASN.1 grammars");
            Console.Error.WriteLine("\t\t\t\tusing the default encoding properties");
            //Console.Error.WriteLine("\t -BER\t\t\tgenerates encoding and decoding functions for.");
            //Console.Error.WriteLine("\t\t\t\tBasic Encoding Rules (BER)");
            //Console.Error.WriteLine();
            //Console.Error.WriteLine("\t -XER\t\t\tgenerates encoding and decoding functions for");
            //Console.Error.WriteLine("\t\t\t\tXML Encoding Rules");
            //Console.Error.WriteLine();
            Console.Error.WriteLine("\t -ast file.xml          Produces an XML file of the parsed input ASN.1");
            Console.Error.WriteLine("\t\t\t\tgrammar.(No encoders/decoders are produced)");
            
            Console.Error.WriteLine("\t -customStg stgFile.stg:outputFile");
            Console.Error.WriteLine("\t\t\t\tInvokes the custom stg file 'stgFile.stg' and produces the");
            Console.Error.WriteLine("\t\t\t\toutput file 'outputFile'");

            Console.Error.WriteLine("\t -customStgAstVersion astVersionNumber");
            Console.Error.WriteLine("\t\t\t\twhere astVersionNumber is:");
            Console.Error.WriteLine("\t\t\t\t1\tparameterized types have been removed");
            Console.Error.WriteLine("\t\t\t\t2\tinner types have been removed");
            Console.Error.WriteLine("\t\t\t\t3\tconstraint reference types have been removed");
            Console.Error.WriteLine("\t\t\t\t4\tEnumerated names are unique (required by C backend)");

            Console.Error.WriteLine("\t -customIcdUper stgFile.stg:outputFile");
            Console.Error.WriteLine("\t\t\t\tInvokes the custom stg file 'stgFile.stg' using the icdUper backend");
            Console.Error.WriteLine("\t\t\t\tand produces the output file 'outputFile'");

            Console.Error.WriteLine("\t -customIcdAcn stgFile.stg:outputFile");
            Console.Error.WriteLine("\t\t\t\tInvokes the custom stg file 'stgFile.stg' using the icdAcn backend");
            Console.Error.WriteLine("\t\t\t\tand produces the output file 'outputFile'");


            Console.Error.WriteLine("\t -icdUper file.html     Produces an Interface Control Document for");
            Console.Error.WriteLine("\t\t\t\tthe input ASN.1 grammar for uPER encoding");
            Console.Error.WriteLine("\t -icdAcn file.html      Produces an Interface Control Document for ");
            Console.Error.WriteLine("\t\t\t\tthe input ASN.1 and ACN grammars for ACN encoding");
            Console.Error.WriteLine("\t -typePrefix prefix\tadds 'prefix' to all generated C or Ada/SPARK data types.");
            Console.Error.WriteLine("\t -o outdir\t\tdirectory where all files are produced.");
            Console.Error.WriteLine("\t\t\t\tDefault is current directory");
            Console.Error.WriteLine("\t -atc\t\t\tcreate automatic test cases.");
            Console.Error.WriteLine("\t -equal \t\tGenerate functions for testing type equality");
            Console.Error.WriteLine("\t\t\t\tWhen using Ada, compiler must support Ada2012");
            Console.Error.WriteLine("\t -renamePolicy policy\tSpecify rename policy for enums");
            Console.Error.WriteLine("\t\t\t\t0 no rename (Ada default)");
            Console.Error.WriteLine("\t\t\t\t1 rename only conflicting enumerants (C default)");
            Console.Error.WriteLine("\t\t\t\t2 rename all enumerants of an enum with at lest one conflicting enumerant");

            Console.Error.WriteLine("\t -mfm moduleName");
            Console.Error.WriteLine("\t\t\t\tSpecifies the C header file name or Ada module name");
            Console.Error.WriteLine("\t\t\t\twthat contains the ACN mapping functions'");


            Console.Error.WriteLine();
            Console.Error.WriteLine();
            Console.Error.WriteLine("Example:");
            Console.Error.WriteLine();
            Console.Error.WriteLine("\t{0} -Ada -uPER MyFile.asn", procName);
            return 4;
        }

        public static void WriteTextFile(string fileName, string data, bool makeItExecutable=false)
        {
            System.IO.File.WriteAllText(fileName, data.Replace("\r",""));

            if (makeItExecutable)
            {
                try
                {
                    File.SetAttributes(fileName, (FileAttributes)( (uint)File.GetAttributes(fileName) | 0x80000000));
                }
                catch (Exception)
                {
                }
            }
        }
    }
}
