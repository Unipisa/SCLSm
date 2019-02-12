
using System;
using System.IO;
using System.Collections;



//-----------------------------------------------------------------------------------
// Scanner
//-----------------------------------------------------------------------------------
public partial class Scanner {

	const int maxT = 23;
	const int noSym = 23;


	static Scanner() {
		start = new Hashtable(128);
		for (int i = 65; i <= 90; ++i) start[i] = 1;
		for (int i = 97; i <= 122; ++i) start[i] = 1;
		for (int i = 48; i <= 57; ++i) start[i] = 3;
		start[36] = 15; 
		start[35] = 14; 
		start[45] = 16; 
		start[58] = 17; 
		start[40] = 18; 
		start[44] = 19; 
		start[41] = 20; 
		start[42] = 21; 
		start[124] = 22; 
		start[91] = 23; 
		start[93] = 24; 
		start[46] = 25; 
		start[Buffer.EOF] = -1;

	}
	
	void NextCh() {
		if (oldEols > 0) { ch = EOL; oldEols--; } 
		else {
			pos = buffer.Pos;
			ch = buffer.Read(); col++;
			// replace isolated '\r' by '\n' in order to make
			// eol handling uniform across Windows, Unix and Mac
			if (ch == '\r' && buffer.Peek() != '\n') ch = EOL;
			if (ch == EOL) { line++; col = 0; }
		}

	}

	void AddCh() {
		if (tlen >= tval.Length) {
			char[] newBuf = new char[2 * tval.Length];
			Array.Copy(tval, 0, newBuf, 0, tval.Length);
			tval = newBuf;
		}
		if (ch != Buffer.EOF) {
			tval[tlen++] = (char) ch;
			NextCh();
		}
	}



	bool Comment0() {
		int level = 1, pos0 = pos, line0 = line, col0 = col;
		NextCh();
		if (ch == '/') {
			NextCh();
			for(;;) {
				if (ch == 10) {
					level--;
					if (level == 0) { oldEols = line - line0; NextCh(); return true; }
					NextCh();
				} else if (ch == Buffer.EOF) return false;
				else NextCh();
			}
		} else {
			buffer.Pos = pos0; NextCh(); line = line0; col = col0;
		}
		return false;
	}

	bool Comment1() {
		int level = 1, pos0 = pos, line0 = line, col0 = col;
		NextCh();
		if (ch == '*') {
			NextCh();
			for(;;) {
				if (ch == '*') {
					NextCh();
					if (ch == '/') {
						level--;
						if (level == 0) { oldEols = line - line0; NextCh(); return true; }
						NextCh();
					}
				} else if (ch == '/') {
					NextCh();
					if (ch == '*') {
						level++; NextCh();
					}
				} else if (ch == Buffer.EOF) return false;
				else NextCh();
			}
		} else {
			buffer.Pos = pos0; NextCh(); line = line0; col = col0;
		}
		return false;
	}


	void CheckLiteral() {
		switch (t.val) {
			case "rules": t.kind = 6; break;
			case "term": t.kind = 7; break;
			case "patterns": t.kind = 8; break;
			case "exclude": t.kind = 9; break;
			case "loop": t.kind = 11; break;
			case "ALL": t.kind = 12; break;
			default: break;
		}
	}

	Token NextToken() {
		while (ch == ' ' ||
			ch >= 9 && ch <= 10 || ch == 13
		) NextCh();
		if (ch == '/' && Comment0() ||ch == '/' && Comment1()) return NextToken();
		t = new Token();
		t.pos = pos; t.col = col; t.line = line; 
		int state;
		if (start.ContainsKey(ch)) { state = (int) start[ch]; }
		else { state = 0; }
		tlen = 0; AddCh();
		
		switch (state) {
			case -1: { t.kind = eofSym; break; } // NextCh already done
			case 0: { t.kind = noSym; break; }   // NextCh already done
			case 1:
				if (ch == '!' || ch >= '%' && ch <= '&' || ch == '+' || ch == '-' || ch >= '/' && ch <= '9' || ch == '=' || ch == '?' || ch >= 'A' && ch <= 'Z' || ch >= '^' && ch <= '_' || ch >= 'a' && ch <= 'z' || ch == 163 || ch == 167 || ch == 176 || ch == 231 || ch == 233) {AddCh(); goto case 2;}
				else {t.kind = 1; t.val = new String(tval, 0, tlen); CheckLiteral(); return t;}
			case 2:
				if (ch == '!' || ch >= '%' && ch <= '&' || ch == '+' || ch == '-' || ch >= '/' && ch <= '9' || ch == '=' || ch == '?' || ch >= 'A' && ch <= 'Z' || ch >= '^' && ch <= '_' || ch >= 'a' && ch <= 'z' || ch == 163 || ch == 167 || ch == 176 || ch == 231 || ch == 233) {AddCh(); goto case 2;}
				else {t.kind = 1; t.val = new String(tval, 0, tlen); CheckLiteral(); return t;}
			case 3:
				if (ch >= '0' && ch <= '9') {AddCh(); goto case 7;}
				else if (ch == '.') {AddCh(); goto case 4;}
				else {t.kind = 2; break;}
			case 4:
				if (ch >= '0' && ch <= '9') {AddCh(); goto case 5;}
				else {t.kind = noSym; break;}
			case 5:
				if (ch >= '0' && ch <= '9') {AddCh(); goto case 6;}
				else {t.kind = 2; break;}
			case 6:
				if (ch >= '0' && ch <= '9') {AddCh(); goto case 6;}
				else {t.kind = 2; break;}
			case 7:
				if (ch >= '0' && ch <= '9') {AddCh(); goto case 7;}
				else if (ch == '.') {AddCh(); goto case 4;}
				else {t.kind = 2; break;}
			case 8:
				if (ch == ':') {AddCh(); goto case 9;}
				else {t.kind = noSym; break;}
			case 9:
				{t.kind = 3; break;}
			case 10:
				if (ch == ':') {AddCh(); goto case 11;}
				else {t.kind = noSym; break;}
			case 11:
				{t.kind = 4; break;}
			case 12:
				if (ch == ':') {AddCh(); goto case 13;}
				else {t.kind = noSym; break;}
			case 13:
				{t.kind = 5; break;}
			case 14:
				{t.kind = 10; break;}
			case 15:
				if (ch == 't') {AddCh(); goto case 8;}
				else if (ch == 's') {AddCh(); goto case 10;}
				else if (ch == 'e') {AddCh(); goto case 12;}
				else {t.kind = noSym; break;}
			case 16:
				{t.kind = 13; break;}
			case 17:
				{t.kind = 14; break;}
			case 18:
				{t.kind = 15; break;}
			case 19:
				{t.kind = 16; break;}
			case 20:
				{t.kind = 17; break;}
			case 21:
				{t.kind = 18; break;}
			case 22:
				{t.kind = 19; break;}
			case 23:
				{t.kind = 20; break;}
			case 24:
				{t.kind = 21; break;}
			case 25:
				{t.kind = 22; break;}

		}
		t.val = new String(tval, 0, tlen);
		return t;
	}
} // end Scanner

