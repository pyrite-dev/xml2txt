program xml2txt;

uses
	DOM,
	XMLRead,
	Sysutils,
	Strutils,
	Math,
	RegExpr,
	Classes;

const
	PaperWidth : Integer = 72;
	PaperHeight : Integer = 58;
	PaddingLeft : Integer = 3;
	NL : String = #10;

var
	Src : TXMLDocument;
	ErrCount : Integer;

	MetaNode : TDOMNode;
	DateNode : TDOMNode;
	TitleNode : TDOMNode;
	ShortNode : TDOMNode;
	MetaOrgNode : TDOMNode;
	AuthorsNode : TDOMNode;

	TocNode : TDOMNode;

	(* These 4 nodes are used temporary *)
	AuthorNode : TDOMNode;
	NameNode : TDOMNode;
	OrgNode : TDOMNode;
	EmailNode : TDOMNode;

	LineN : Integer;
	PageN : Integer;

	SectionN : Array[1..10] of Integer;

	TempNode : TDOMNode;
	TempContentNode : TDOMNode;

	Subtr : Integer;
	AtStr : String;
	Subtr2 : Integer;
	AtStr2 : String;

	FirstTime : Boolean;

	Buffer : String;

	HaveToc : Boolean;
	
	TocCount : Integer;

	TocPrefix : String;

	TocRegExpr : TRegExpr;

procedure AddLine(S : String);
begin
	Buffer := Buffer + S + NL;
end;

procedure OutputStr(S : String); forward;

procedure NewPage(HaveTitle : Boolean);
var
	I : Integer;
	L : Integer;
	S : String;
begin
	Inc(PageN);
	S := '[Page ' + IntToStr(PageN) +']';
	for I := 1 to (PaperHeight - LineN) do
	begin
		AddLine('');
	end;
	AddLine(String(AuthorsNode.TextContent) + StringOfChar(' ', PaperWidth - Length(S) - Length(AuthorsNode.TextContent)) + S);
	LineN := 0;

	if HaveTitle then
	begin
		L := Floor(PaperWidth / 2 - Length(TitleNode.TextContent) / 2) - Length(ShortNode.TextContent);
		S := String(ShortNode.TextContent);
		S := S + StringOfChar(' ', L) + String(TitleNode.TextContent);
		S := S + StringOfChar(' ', (PaperWidth - L - Length(TitleNode.TextContent) - Length(ShortNode.TextContent)) - Length(DateNode.TextContent)) + String(DateNode.TextContent);
		OutputStr(S);
		OutputStr('');
		OutputStr('');
	end;
end;

procedure NewPage();
begin
	NewPage(True);
end;

procedure OutputStr(S : String);
begin
	AddLine(S);
	Inc(LineN);
	if LineN = PaperHeight then
	begin
		NewPage();
	end;
end;

procedure OutputStr(S : UnicodeString);
begin
	OutputStr(String(S));
end;

procedure OutputStr();
begin
	OutputStr('');
end;

function Validate() : Integer;
var
	Child : TDOMNode;
begin
	Validate := 0;
	if not(Src.DocumentElement.NodeName = 'specification') then
	begin
		WriteLn(StdErr, 'ERR Root element must be <specification>');
		Inc(Validate);
	end;

	MetaNode := Src.DocumentElement.FindNode('meta');
	if not(Assigned(MetaNode)) then
	begin
		WriteLn(StdErr, 'ERR <meta> missing');
		Inc(Validate);
	end
	else
	begin
		DateNode := MetaNode.FindNode('date');
		if not(Assigned(DateNode)) then
		begin
			WriteLn(StdErr, 'ERR <meta>.<date> missing');
			Inc(Validate);
		end;

		TitleNode := MetaNode.FindNode('title');
		if not(Assigned(TitleNode)) then
		begin
			WriteLn(StdErr, 'ERR <meta>.<title> missing');
			Inc(Validate);
		end;

		ShortNode := MetaNode.FindNode('short');
		if not(Assigned(ShortNode)) then
		begin
			WriteLn(StdErr, 'ERR <meta>.<short> missing');
			Inc(Validate);
		end;

		MetaOrgNode := MetaNode.FindNode('organization');
		if not(Assigned(MetaOrgNode)) then
		begin
			WriteLn(StdErr, 'ERR <meta>.<organization> missing');
			Inc(Validate);
		end;

		AuthorsNode := MetaNode.FindNode('authors');
		if not(Assigned(AuthorsNode)) then
		begin
			WriteLn(StdErr, 'ERR <meta>.<authors> missing');
			Inc(Validate);
		end;
	end;

	Child := Src.DocumentElement.FirstChild;
	while Assigned(Child) do
	begin
		if Child.NodeName = 'author' then
		begin
			if not(Assigned(TDOMElement(Child).FindNode('name'))) then
			begin
				WriteLn(StdErr, 'ERR <author>.<name> missing');
				Inc(Validate);
			end;
			if not(Assigned(TDOMElement(Child).FindNode('organization'))) then
			begin
				WriteLn(StdErr, 'ERR <author>.<organization> missing');
				Inc(Validate);
			end;
		end;
		Child := Child.NextSibling;
	end;
end;

procedure Section(Node : TDOMNode; Numbered : Boolean; InitialIndent : Integer; Nest : Integer);
var
	Indent : Integer;
	R : TStringList;
	Lines : TStringList;
	I : Integer;
	J : Integer;
	B : Boolean;
	S : String;
	IncrAmount : Integer;
begin
	Indent := InitialIndent + PaddingLeft;

	Lines := TStringList.Create();

	R := TStringList.Create();
	SplitRegExpr('\r?\n', String(Node.TextContent), R);
	B := False;
	for I := 0 to (R.Count - 1) do
	begin
		R[I] := Trim(R[I]);
		if Length(R[I]) > 0 then
		begin
			B := True;
		end;

		if B then
		begin
			Lines.Add(R[I]);
		end;
	end;

	B := False;
	for I := (Lines.Count - 1) downto 0 do
	begin
		if Length(Lines[I]) > 0 then
		begin
			break;
		end;
		Lines.Delete(I);
	end;
	R.Free();

	for I := 0 to (Lines.Count - 1) do
	begin
		S := '';
		R := TStringList.Create();
		SplitRegExpr('[ \t]+', Lines[I], R);
		for J := 0 to (R.Count - 1) do
		begin
			IncrAmount := 1;
			if Length(S) = 0 then
			begin
				IncrAmount := 0;
			end;
			if (Length(R[J]) + IncrAmount + Length(S)) > (PaperWidth - Indent) then
			begin
				OutputStr(StringOfChar(' ', Indent) + S);
				S := '';
			end;
			if Length(S) = 0 then
			begin
				S := R[J];
			end
			else
			begin
				S := S + ' ' + R[J];
			end;
		end;
		OutputStr(StringOfChar(' ', Indent) + S);
		R.Free();
	end;
	OutputStr();

	Lines.Free();
end;

procedure InitSectionNumber();
var
	I : Integer;
begin
	for I := 1 to Length(SectionN) do
	begin
		SectionN[I] := 0;
	end;
end;

procedure ScanSections(Root : TDOMNode; Numbered : Boolean; Indent : Integer; Nest : Integer); forward;

procedure SectionName(Node : TDOMNode; Numbered : Boolean; Indent : Integer; Nest : Integer);
var
	SectionNumber : String;
	I : Integer;
begin
	SectionNumber := '';

	if Numbered then
	begin
		Inc(SectionN[Nest]);
		for I := 1 to Nest do
		begin
			SectionNumber := SectionNumber + IntToStr(SectionN[I]) + '.';
		end;
		SectionNumber := SectionNumber + ' ';
	end;

	if TDOMElement(Node).HasAttribute('name') then
	begin
		if (PaperHeight - LineN) <= 5 then begin
			NewPage();
		end;

		OutputStr(SectionNumber + String(TDOMElement(Node).GetAttribute('name')));
		OutputStr();
	end;
end;

procedure ScanSection(Node : TDOMNode; Numbered : Boolean; Indent : Integer; Nest : Integer);
var
	Child : TDOMNode;
	MoreIndent : Integer;
begin
	if (Node.NodeName = 'section') or (Node.NodeName = 'indent-section') then
	begin
		MoreIndent := 0;
		if Node.NodeName = 'indent-section' then
		begin
			MoreIndent := PaddingLeft;
		end;
		Child := Node.FirstChild;
		SectionName(Node, Numbered, Indent + MoreIndent, Nest);
		while Assigned(Child) do
		begin
			if Child.NodeName = 'content' then
			begin
				Section(Child, Numbered, Indent + MoreIndent, Nest);
			end
			else
			begin
				ScanSection(Child, Numbered, Indent + MoreIndent, Nest + 1);
			end;
			Child := Child.NextSibling;
		end;
	end
	else if Node.NodeName = 'numbered' then
	begin
		InitSectionNumber();
		ScanSections(Node, True, Indent, Nest);
	end
	else if Node.NodeName = 'toc' then
	begin
		HaveToc := True;
	end
	else if Node.NodeName = 'newpage' then
	begin
		NewPage();
	end;
end;

procedure ScanSections(Root : TDOMNode; Numbered : Boolean; Indent : Integer; Nest : Integer);
var
	Child : TDOMNode;
begin
	Child := Root.FirstChild;
	while Assigned(Child) do
	begin
		ScanSection(Child, Numbered, Indent, Nest);
		Child := Child.NextSibling;
	end;
end;

procedure Render();
begin
	LineN := 0;
	PageN := 0;
	Buffer := '';
	TempNode := Src.DocumentElement.FirstChild;
	FirstTime := True;
	while Assigned(TempNode) do
	begin
		if TempNode.NodeName = 'author' then
		begin
			Subtr := 0;
			AtStr := '';
			Subtr2 := 0;
			AtStr2 := '';
			if FirstTime then
			begin
				FirstTime := False;
				Subtr := Length(MetaOrgNode.TextContent);
				AtStr := String(MetaOrgNode.TextContent);
			end;
			NameNode := TempNode.FindNode('name');
			OrgNode := TempNode.FindNode('organization');
			OutputStr(AtStr + StringOfChar(' ', PaperWidth - Length(NameNode.TextContent) - Subtr) + String(NameNode.TextContent));
			OutputStr(AtStr2 + StringOfChar(' ', PaperWidth - Length(OrgNode.TextContent) - Subtr2) + String(OrgNode.TextContent));
		end;
		TempNode := TempNode.NextSibling;
	end;
	OutputStr(StringOfChar(' ', PaperWidth - Length(DateNode.TextContent)) + String(DateNode.TextContent));

	OutputStr();
	OutputStr(StringOfChar(' ', Floor(PaperWidth / 2 - Length(TitleNode.TextContent) / 2)) + String(TitleNode.TextContent));
	OutputStr();

	TempNode := Src.CreateElement('newpage');
	Src.DocumentElement.AppendChild(TempNode);

	TempNode := Src.CreateElement('section');
	TempContentNode := Src.CreateElement('content');
	TDOMElement(TempNode).SetAttribute('name', 'Authors'' Addresses');
	AuthorNode := Src.DocumentElement.FirstChild;
	while Assigned(AuthorNode) do
	begin
		if AuthorNode.NodeName = 'author' then
		begin
			NameNode := AuthorNode.FindNode('name');
			OrgNode := AuthorNode.FindNode('organization');
			EmailNode := AuthorNode.FindNode('email');

			TempContentNode.TextContent := UnicodeString(String(TempContentNode.TextContent) + String(NameNode.TextContent) + NL);
			TempContentNode.TextContent := UnicodeString(String(TempContentNode.TextContent) + String(OrgNode.TextContent) + NL + NL);
			if Assigned(EmailNode) then
			begin
				TempContentNode.TextContent := UnicodeString(String(TempContentNode.TextContent) + 'Email: ' + String(EmailNode.TextContent) + NL + NL + NL);
			end;
		end;
		AuthorNode := AuthorNode.NextSibling;
	end;
	TempNode.AppendChild(TempContentNode);
	Src.DocumentElement.AppendChild(TempNode);

	ScanSections(Src.DocumentElement, False, 0, 1);

	if not(LineN = 0) then
	begin
		NewPage(False);
	end;
end;

function CountNumbered(Root : TDOMNode; DoCount : Boolean; Base : Integer) : Integer;
var
	Child : TDOMNode;
begin
	CountNumbered := 0;
	Child := Root.FirstChild;
	while Assigned(Child) do
	begin
		if Child.NodeName = 'numbered' then
		begin
			CountNumbered := CountNumbered + CountNumbered(Child, True, CountNumbered);
		end
		else if (Child.NodeName = 'section') and DoCount then
		begin
			CountNumbered := CountNumbered + CountNumbered(Child, True, CountNumbered);
			Inc(CountNumbered);
		end;
		Child := Child.NextSibling;
	end;
end;

begin
	HaveToc := False;

	if not(ParamCount = 1) then
	begin
		WriteLn('Invalid arguments');
		Halt(1);
	end;

	if not(FileExists(ParamStr(1))) then
	begin
		WriteLn('File does not exist');
		Halt(1);
	end;

	ReadXMLFile(Src, ParamStr(1));
	ErrCount := Validate();
	if ErrCount > 0 then
	begin
		WriteLn(StdErr, IntToStr(ErrCount) + ' error(s)');
		Halt(1);
	end;
	Render();
	if HaveToc then
	begin
		TocNode := Src.DocumentElement.FindNode('toc');
		
		TempNode := Src.CreateElement('section');
		TempContentNode := Src.CreateElement('content');

		for TocCount := 1 to CountNumbered(Src.DocumentElement, False, 1) do
		begin
			TocPrefix := StringOfChar('#', PaperWidth - 9 - Length(IntToStr(TocCount)) - PaddingLeft);
			TempContentNode.TextContent := UnicodeString(String(TempContentNode.TextContent) + TocPrefix + 'TOC-INDEX' + IntToStr(TocCount) + NL);
		end;

		TDOMElement(TempNode).SetAttribute('name', 'Table of Contents');
		TempNode.AppendChild(TempContentNode);

		Src.DocumentElement.InsertBefore(TempNode, TocNode);
		Render();

		for TocCount := 1 to CountNumbered(Src.DocumentElement, False, 1) do
		begin
			TocRegExpr := TRegExpr.Create('#+TOC-INDEX' + IntToStr(TocCount));
			TocRegExpr.Exec(Buffer);

			Buffer := TocRegExpr.Replace(Buffer, '');

			TocRegExpr.Free();
		end;
	end;
	Write(Buffer);
	Src.Free();
end.
