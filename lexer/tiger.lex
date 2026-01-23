type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentCount = ref 0

val curString = ref ""
val stringStart = ref 0
fun stringAdd s = (curString := !curString ^ s)


fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 
digit=[0-9];
alpha=[A-Za-z];
id={alpha}({alpha}|{digit}|"_")*;
%s COMMENT STRING ESCAPE;

%%

\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
[ \t\r]+ => (continue());

<INITIAL>"/*" => (YYBEGIN COMMENT; commentCount := 1; continue());
<COMMENT>"/*" =>(commentCount := !commentCount + 1; continue());
<COMMENT>"*/" => (commentCount := !commentCount -1; if !commentCount = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT>. => (continue());

<INITIAL>"\"" => (YYBEGIN STRING; curString := ""; stringStart := yypos; continue());
<STRING>[^"\\\n]+ => (stringAdd yytext; continue());
<STRING>. => (stringAdd yytext; continue());

<STRING>"\\" => (YYBEGIN ESCAPE; continue());

<ESCAPE>"n" => (stringAdd "\n"; YYBEGIN STRING; continue());
<ESCAPE>"t" => (stringAdd "\t"; YYBEGIN STRING; continue());
<ESCAPE>"\"" => (stringAdd "\""; YYBEGIN STRING; continue());
<ESCAPE>"\\" => (stringAdd "\\"; YYBEGIN STRING; continue());
<ESCAPE>"^"[@-_] => (stringAdd (String.str (String.sub(yytext, 1))); YYBEGIN STRING; continue());
<ESCAPE>{digit}{3} =>  (stringAdd (String.str (Char.chr (valOf (Int.fromString yytext)))); YYBEGIN STRING; continue());
<ESCAPE>[ \t\n\r\f]+"\\" => (YYBEGIN STRING; continue());
<ESCAPE>. => (ErrorMsg.error yypos ("Illegal Escape \\" ^ yytext); YYBEGIN STRING; continue());

<STRING>\n => (ErrorMsg.error yypos "Newline in string literal"; YYBEGIN INITIAL; continue());
<STRING>"\"" =>  (YYBEGIN INITIAL; Tokens.STRING(!curString, !stringStart, yypos+1));

<INITIAL>"type" => (Tokens.TYPE(yypos, yypos+4));
<INITIAL>"var" => (Tokens.VAR(yypos, yypos+3));
<INITIAL>"function" => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>"break" => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>"of" => (Tokens.OF(yypos, yypos+2));
<INITIAL>"end" => (Tokens.END(yypos, yypos+3));
<INITIAL>"in" => (Tokens.IN(yypos, yypos+2));
<INITIAL>"nil" => (Tokens.NIL(yypos, yypos+3));
<INITIAL>"let" => (Tokens.LET(yypos, yypos+3));
<INITIAL>"do" => (Tokens.DO(yypos, yypos+2));
<INITIAL>"to" => (Tokens.TO(yypos, yypos+2));
<INITIAL>"for" => (Tokens.FOR(yypos, yypos+3));
<INITIAL>"while" => (Tokens.WHILE(yypos, yypos+5));
<INITIAL>"else" => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>"then" => (Tokens.THEN(yypos, yypos+4));
<INITIAL>"if" => (Tokens.IF(yypos, yypos+2));
<INITIAL>"array" => (Tokens.ARRAY(yypos, yypos+5));
<INITIAL>":=" => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL>"|" => (Tokens.OR(yypos, yypos+1));
<INITIAL>"&" => (Tokens.AND(yypos, yypos+1));
<INITIAL>">=" => (Tokens.GE(yypos, yypos+2));
<INITIAL>">" => (Tokens.GT(yypos, yypos+1));
<INITIAL>"<=" => (Tokens.LE(yypos, yypos+2));
<INITIAL>"<" => (Tokens.LT(yypos, yypos+1));
<INITIAL>"<>" => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>"=" => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"/" => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>"*" => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"+" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"." => (Tokens.DOT(yypos, yypos+1));
<INITIAL>"{" => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>"}" => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL>"[" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>"]" => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>"(" => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>")" => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL>";" => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL>":" => (Tokens.COLON(yypos, yypos+1));
<INITIAL>"," => (Tokens.COMMA(yypos, yypos+1));
<INITIAL>{digit}+ => (Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos+size yytext));
<INITIAL>{id} => (Tokens.ID(yytext, yypos, yypos+size yytext));

.    => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
