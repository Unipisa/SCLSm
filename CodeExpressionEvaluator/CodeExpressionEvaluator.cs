using System;
using System.Collections;
//using System.Reflection;
using SCLS;
//using SCLS.Occurrences;
using System.Text;
using System.Text.RegularExpressions;

namespace SCLS
{
    public class SyntaxPreprocessor
    {
        public static SymbolTable symbols = null;
        public static string ReplaceOccFunctionCall(Match m)
        // Replace each Regex cc match with the number of the occurrence.
        {
            StringBuilder sb = new StringBuilder();
            for (int i = 1; i < m.Groups.Count; i++)
            {
                Group GroupObj = m.Groups[i];
                if (GroupObj.Success)
                {
                    switch (i)
                    {
                        case 1:
                            sb.Append(" (Occurrences.occ( new int[] {" 
                                        + TOOLS.preprocess_parameter(GroupObj.Value, symbols) 
                                        + "},");
                            break;
                        case 2:
                            sb.Append(" new int[] {"
                                        + TOOLS.preprocess_parameter(GroupObj.Value, symbols) 
                                        + "},b)) ");
                            break;
                    }
                }
            }
            return sb.ToString();
        }
        public static string ReplaceMathFunctionCall(Match m)
        // Replace each Regex cc match with the number of the occurrence.
        {
            StringBuilder sb = new StringBuilder();
            for (int i = 1; i < m.Groups.Count; i++)
            {
                Group GroupObj = m.Groups[i];
                if (GroupObj.Success)
                {
                    switch (i)
                    {
                        case 1:
                            sb.Append(" (Math."
                                        + GroupObj.Value
                                        + "(");
                            break;
                        case 2:
                            sb.Append(GroupObj.Value + "))");
                            break;
                    }
                }
            }
            return sb.ToString();
        }
        public static string PreprocessSource(string src, Object st) 
        {
            symbols = (SCLS.SymbolTable) st;
            //crea regex
            //try
            //{
                Regex RegexObj = new Regex("occ\\((?<what>.*?),(?<where>.*?)\\)", RegexOptions.IgnoreCase | RegexOptions.Compiled);
                src = RegexObj.Replace(src, ReplaceOccFunctionCall);
            //}
            //catch (ArgumentException ex)
            //{
                // Syntax error in the regular expression
            //}

            //crea regex
            //try
            //{
                RegexObj = new Regex("(Abs|Ceiling|Exp|Floor|IEEERemainder|Log|Log10|Max|Min|Pow|Round|Sign|Sqrt|Acos|Asin|Atan|Atan2|Cos|Cosh|Sin|Sinh|Tan|Tanh)\\((?<par>.*?)\\)", RegexOptions.IgnoreCase | RegexOptions.Compiled);
                src = RegexObj.Replace(src, ReplaceMathFunctionCall);
            //}
            //catch (ArgumentException ex)
            //{
                // Syntax error in the regular expression
            //}
            return src;
        }
    }

    ///here can put a MathSyntaxPreprocessor that replace alla call to (Abs|..) all Math method with Math.method

    public class CSharpCodeExpressionEvaluator
    {
        public object myobj = null;
        public ArrayList errorMessages;


        public CSharpCodeExpressionEvaluator()
        {
            errorMessages = new ArrayList(); 
        }
        public bool init(string expr)
        {
            Environment.CurrentDirectory = System.AppDomain.CurrentDomain.BaseDirectory;

            Microsoft.CSharp.CSharpCodeProvider cp = new Microsoft.CSharp.CSharpCodeProvider();
            //System.CodeDom.Compiler.ICodeCompiler ic = cp.CreateCompiler();
            System.CodeDom.Compiler.CompilerParameters cpar = new System.CodeDom.Compiler.CompilerParameters();
            cpar.GenerateInMemory = true;
            cpar.GenerateExecutable = false;
            cpar.ReferencedAssemblies.Add("system.dll");
            cpar.ReferencedAssemblies.Add(@"scls.dll");
            string src = 
            @"using System; 
              using SCLS;
              using SCLS.Occurrences; 
              class myclass 
              { 
                public myclass(){} 
                public static double eval(bindings b) 
                { 
                    return (" + expr + "); " +
               @"}  
             } ";
            System.CodeDom.Compiler.CompilerResults cr = cp.CompileAssemblyFromSource(cpar, src);
            //= ic.CompileAssemblyFromSource(cpar, src);
            foreach (System.CodeDom.Compiler.CompilerError ce in cr.Errors)
            {
                errorMessages.Add(ce.ErrorText); Console.WriteLine(ce.ErrorText);//DEBUG
            }

            // here mono return null without errors //cr.CompiledAssembly = null
            //Console.WriteLine("src:");
            //Console.WriteLine(src);
            //Console.WriteLine("cp:");
            //Console.WriteLine(cp);
            //Console.WriteLine("Cr:");
            //Console.WriteLine(cr);
            //Console.WriteLine("cr.CompiledAssembly :");
            //fConsole.WriteLine(cr.CompiledAssembly);
            


            if (cr.Errors.Count == 0 && cr.CompiledAssembly != null)
            {
                Type ObjType = cr.CompiledAssembly.GetType("myclass");
                try
                {
                    if (ObjType != null)
                    {
                        myobj = Activator.CreateInstance(ObjType);
                    }
                }
                catch (Exception ex)
                {
                    errorMessages.Add(ex.Message);
                    Console.WriteLine(ex.Message);//DEBUG
                }
                return true;
            }
            else
                return false;
        }
        public double evalRateFunction(Object binding)//SCLS.bindings binding)
        {
            double val = 0.0;
            Object[] myParams = new Object[1] { (SCLS.bindings)binding};
            if (myobj != null)
            {
                System.Reflection.MethodInfo evalMethod = myobj.GetType().GetMethod("eval");
                val = (double)evalMethod.Invoke(myobj, myParams);
            }
            return val;
        }
    }

// here the version for f# code
// they need -R "fscodeprovider.dll" // still not mature 

//    public class FSharpCodeExpressionEvaluator
//    {
//        public object myobj = null;
//        public ArrayList errorMessages;

//        public FSharpCodeExpressionEvaluator()
//        {
//            errorMessages = new ArrayList();
//        }
//        public bool init(string expr)
//        {
//            EeekSoft.FSharp.CodeDom.FSharpCodeProvider cp = new EeekSoft.FSharp.CodeDom.FSharpCodeProvider();
//            //System.CodeDom.Compiler.ICodeCompiler ic = cp.CreateCompiler();
//            System.CodeDom.Compiler.CompilerParameters cpar = new System.CodeDom.Compiler.CompilerParameters();
//            cpar.GenerateInMemory = true;
//            cpar.GenerateExecutable = false;
//            cpar.ReferencedAssemblies.Add("system.dll");
//            cpar.ReferencedAssemblies.Add(@"../../../SCLS/scls.dll");
//            string src = @"
//            #ligth
//            type myFclass =
//                class
//                    new() = {}
//                    static member evalKonst() : int = 33
//                end
//            ";
//            /*
//             string src = 
//             "using System; using SCLS; using SCLS.SCLS;" +
//             "class myclass " +
//             "{ " +
//             "public myclass(){} " +
//             "public static double eval(bindings b) " +
//             "{ " +
//             "return " + expr + "; " +
//             "} " +
//             "public static double evalKonst() " +
//             "{ " +
//             "return " + expr + "; " +
//             "} " +
//             "} ";*/
//            System.CodeDom.Compiler.CompilerResults cr = cp.CompileAssemblyFromSource(cpar, src);
//            //= ic.CompileAssemblyFromSource(cpar, src);
//            foreach (System.CodeDom.Compiler.CompilerError ce in cr.Errors)
//            {
//                errorMessages.Add(ce.ErrorText); Console.WriteLine(ce.ErrorText);//DEBUG
//            }

//            if (cr.Errors.Count == 0 && cr.CompiledAssembly != null)
//            {
//                Type ObjType = cr.CompiledAssembly.GetType("myFclass");
//                try
//                {
//                    if (ObjType != null)
//                    {
//                        myobj = Activator.CreateInstance(ObjType);
//                    }
//                }
//                catch (Exception ex)
//                {
//                    errorMessages.Add(ex.Message);
//                    Console.WriteLine(ex.Message);//DEBUG
//                }
//                return true;
//            }
//            else
//                return false;
//        }
//        public double eval()
//        {
//            double val = 0.0;
//            Object[] myParams = new Object[0] { };
//            if (myobj != null)
//            {
//                System.Reflection.MethodInfo evalMethod = myobj.GetType().GetMethod("evalKonst");
//                val = (double)evalMethod.Invoke(myobj, myParams);
//                //Console.WriteLine("OK");//DEBUG
//                //Console.WriteLine(val.ToString());//DEBUG
//            }
//            else
//            { //Console.WriteLine("Compilazione non riuscita!");//DEBUG
//            }
//            return val;
//        }
//        public double evalRateFunction(SCLS.SCLS.bindings binding)
//        {
//            double val = 0.0;
//            Object[] myParams = new Object[1] { binding };
//            if (myobj != null)
//            {
//                System.Reflection.MethodInfo evalMethod = myobj.GetType().GetMethod("eval");
//                val = (double)evalMethod.Invoke(myobj, myParams);
//            }
//            return val;
//        }
//    }
}
