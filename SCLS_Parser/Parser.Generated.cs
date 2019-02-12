using SCLSm;
using SCLSm.SCLS;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using Microsoft.FSharp;
using Microsoft.FSharp.Core;

using System;



public partial class Parser {
	public const int _EOF = 0;
	public const int _Id = 1;
	public const int _Number = 2;
	public const int _VART = 3;
	public const int _VARS = 4;
	public const int _VARE = 5;
	public const int _RULESDIC = 6;
	public const int _TERMDIC = 7;
	public const int _PATTERNDIC = 8;
	public const int _EXCLUDEDIC = 9;
	public const int _CODEDELIMITER = 10;
	public const int _LOOP = 11;
	public const int maxT = 23;


public static Model model;



	void Get () {
		for (;;) {
			t = la;
			la = scanner.Scan();
			if (la.kind <= maxT) { ++errDist; break; }

			la = t;
		}
	}
	
	void SCLS() {
		List<Rule> rulesList = new List<Rule>();
		Compartment initialTerm = new Compartment(Option<Option<Node>>.None);
		SymbolTable<string> symbols = new SymbolTable<string>();
		List<Pattern> patternsList = new List<Pattern>();
		List<int> vars = new List<int>();	
		StringBuilder TEMP = new StringBuilder(); 
		List<string> exclude_list = new List<string>();			
		
		Expect(6);
		rules(rulesList, symbols, vars);
		Expect(7);
		terms(initialTerm, symbols, vars);
		if (la.kind == 8) {
			Get();
			if (la.kind == 1 || la.kind == 2) {
				patterns(patternsList, symbols, vars  );
			}
		}
		if (la.kind == 9) {
			Get();
			if (la.kind == 1 || la.kind == 2 || la.kind == 12) {
				if (la.kind == 1 || la.kind == 2) {
					Identifier(TEMP);
					exclude_list.Add(TEMP.ToString()); TEMP = new StringBuilder(); 
					while (la.kind == 1 || la.kind == 2) {
						Identifier(TEMP);
						exclude_list.Add(TEMP.ToString()); TEMP = new StringBuilder(); 
					}
				} else {
					Get();
					foreach (KeyValuePair<string,int> keyVal in symbols.inverse_table) {	exclude_list.Add(keyVal.Key); } 
					
					if (la.kind == 13) {
						Get();
						Identifier(TEMP);
						exclude_list.Remove(TEMP.ToString()); TEMP = new StringBuilder(); 
						while (la.kind == 1 || la.kind == 2) {
							Identifier(TEMP);
							exclude_list.Remove(TEMP.ToString()); TEMP = new StringBuilder(); 
						}
					}
				}
			}
		}
		foreach (Rule r in rulesList) { r.init(symbols);    }
		model = new Model(rulesList.ToArray(), patternsList.ToArray(), initialTerm, symbols,Microsoft.FSharp.Collections.SetModule.of_seq<int>(vars), Microsoft.FSharp.Collections.SetModule.of_seq<string>(exclude_list));	
		
	}

	void rules(List<Rule> rulesList, SymbolTable<string> symbols, List<int> vars  ) {
		
		rule(rulesList, symbols,vars );
		
		if (la.kind == 1 || la.kind == 2) {
			rules(rulesList, symbols,vars );
		}
	}

	void terms(Compartment upperTerm, SymbolTable<string> symbols,List<int> vars  ) {
		Compartment currentTerm = new Compartment(Option<Option<Node>>.None); 
		term(currentTerm, symbols,vars);
		if (la.kind == 18) {
			Get();
			Expect(2);
		}
		long temp;
		long.TryParse(t.val, out temp);
		if(temp == 0) temp++;
		upperTerm.AddChild(currentTerm, temp, Option<bool>.None);
		
		if (la.kind == 19) {
			Get();
			terms(upperTerm, symbols,vars );
		}
	}

	void patterns(List<Pattern> patternsList, SymbolTable<string> symbols, List<int> vars  ) {
		
		pattern(patternsList, symbols,vars );
		
		if (la.kind == 1 || la.kind == 2) {
			patterns(patternsList, symbols,vars );
		}
	}

	void Identifier(StringBuilder TEMP) {
		if (la.kind == 2) {
			Get();
			TEMP.Append(t.val); 
		}
		Expect(1);
		TEMP.Append(t.val); 
	}

	void rule(List<Rule> rulesList, SymbolTable<string> symbols,List<int> vars  ) {
		Compartment left = new Compartment(Option<Option<Node>>.None);
		Compartment right = new Compartment(Option<Option<Node>>.None);	
		StringBuilder TEMP = new StringBuilder();							
		Identifier(TEMP);
		string name = TEMP.ToString();	
		Expect(14);
		Expect(15);
		terms(left, symbols,vars );
		Expect(16);
		
		if (StartOf(1)) {
			terms(right , symbols,vars );
		}
		Expect(16);
		TEMP = new StringBuilder();	
		rateFormula(TEMP);
		Expect(17);
		rulesList.Add(new Rule(name,left, right, TEMP.ToString(), symbols));
	}

	void rateFormula(StringBuilder TEMP) {
		if (la.kind == 10) {
			Get();
			Get();
			TEMP.Append("#" + t.val); 
			while (StartOf(2)) {
				Get();
				TEMP.Append(t.val); 
			}
			Expect(10);
		} else if (StartOf(2)) {
			Get();
			TEMP.Append(t.val); 
		} else SynErr(24);
	}

	void term(Compartment term, SymbolTable<string> symbols,List<int> vars  ) {
		if (StartOf(3)) {
			Sequence seq = new Sequence( Option<List<Element>>.None, Option<Option<Node>>.Some(Option<Node>.Some(term)));
			sequences(seq, symbols,vars );
			term.AddChild(seq, 1,Option<bool>.None);			
		} else if (la.kind == 3) {
			StringBuilder TEMP = new StringBuilder(); 
			Get();
			Identifier(TEMP);
			int el = symbols.add(TEMP.ToString());
			term.AddChild(new TermVariable(el, Option<Node>.Some(term)), 1, Option<bool>.None);
			vars.Add(el);
			
		} else if (la.kind == 11 || la.kind == 15) {
			LoopingSequence seq = new LoopingSequence(Option<List<Element>>.None, Option<Option<Node>>.Some(Option<Node>.None)); 
			Compartment content = new Compartment(Option<Option<Node>>.Some(Option<Node>.None));
			
			
			if (la.kind == 11) {
				Get();
			}
			Expect(15);
			if (StartOf(3)) {
				sequences(seq, symbols,vars);
			}
			Expect(17);
			if (la.kind == 20) {
				enclosedTerm(content, symbols,vars);
			}
			Loop l = new Loop(seq, content, Option<Option<Node>>.Some(Option<Node>.Some(term)));
			term.AddChild(l, 1, Option<bool>.None); 
		} else SynErr(25);
	}

	void sequences(Sequence seq, SymbolTable<string> symbols,List<int> vars  ) {
		int elem; int type; /*int link;*/ 
		sequence(out elem, out type/*, out link*/, symbols,vars );
		switch (type)
		    {
		        case 0: seq.AddChild(new ConstantElement(elem/*, link*/));
		  		break;
		  case 1: seq.AddChild(new ElementVariable(elem/*, link*/));
			vars.Add(elem);
		          break;
		  case 2: seq.AddChild(new SequenceVariable(elem));
			vars.Add(elem);
		          break;
		
		        }
					
		if (la.kind == 22) {
			Get();
			sequences(seq, symbols,vars);
		}
	}

	void enclosedTerm(Compartment term, SymbolTable<string> symbols,List<int> vars 	) {
		Expect(20);
		terms(term, symbols,vars);
		Expect(21);
		
	}

	void sequence(out int elem, out int type/*, out int link*/, SymbolTable<string> symbols,List<int> vars  ) {
		elem = 0; type = 0;/* link = -1*/ ; StringBuilder TEMP = new StringBuilder(); 
		if (la.kind == 1 || la.kind == 2) {
			
			Identifier(TEMP);
			elem = symbols.add(TEMP.ToString()); 
		} else if (la.kind == 5) {
			Get();
			Identifier(TEMP);
			elem = symbols.add(TEMP.ToString()); type = 1;
		} else if (la.kind == 4) {
			Get();
			Identifier(TEMP);
			elem = symbols.add(TEMP.ToString()); type = 2;
		} else SynErr(26);
	}

	void pattern(List<Pattern> patternsList, SymbolTable<string> symbols,List<int> vars  ) {
		Compartment p = new Compartment(Option<Option<Node>>.None);
		StringBuilder TEMP = new StringBuilder();							
																			
		Identifier(TEMP);
		string name = TEMP.ToString();	
		Expect(14);
		Expect(15);
		terms(p, symbols,vars );
		TEMP = new StringBuilder();	
		if (la.kind == 16) {
			Get();
			rateFormula(TEMP);
		}
		Expect(17);
		patternsList.Add(new Pattern(p,name, TEMP.ToString() )); 
	}



	public void Parse() {
		la = new Token();
		la.val = "";		
		Get();
		SCLS();

    Expect(0);
	}
	
	static readonly bool[,] set = {
		{T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x},
		{x,T,T,T, T,T,x,x, x,x,x,T, x,x,x,T, x,x,x,x, x,x,x,x, x},
		{x,T,T,T, T,T,T,T, T,T,x,T, T,T,T,T, T,T,T,T, T,T,T,T, x},
		{x,T,T,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x}

	};
} // end Parser


public partial class Errors {
    private string GetErrorMessage(int n) {
        string s = null;
        switch(n) {
			case 0: s = "EOF expected"; break;
			case 1: s = "Id expected"; break;
			case 2: s = "Number expected"; break;
			case 3: s = "VART expected"; break;
			case 4: s = "VARS expected"; break;
			case 5: s = "VARE expected"; break;
			case 6: s = "RULESDIC expected"; break;
			case 7: s = "TERMDIC expected"; break;
			case 8: s = "PATTERNDIC expected"; break;
			case 9: s = "EXCLUDEDIC expected"; break;
			case 10: s = "CODEDELIMITER expected"; break;
			case 11: s = "LOOP expected"; break;
			case 12: s = "\"ALL\" expected"; break;
			case 13: s = "\"-\" expected"; break;
			case 14: s = "\":\" expected"; break;
			case 15: s = "\"(\" expected"; break;
			case 16: s = "\",\" expected"; break;
			case 17: s = "\")\" expected"; break;
			case 18: s = "\"*\" expected"; break;
			case 19: s = "\"|\" expected"; break;
			case 20: s = "\"[\" expected"; break;
			case 21: s = "\"]\" expected"; break;
			case 22: s = "\".\" expected"; break;
			case 23: s = "??? expected"; break;
			case 24: s = "invalid rateFormula"; break;
			case 25: s = "invalid term"; break;
			case 26: s = "invalid sequence"; break;

        }
        return s;
    }
} // Errors

