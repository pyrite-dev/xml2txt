program xml2txt;

uses
	DOM,
	XMLRead,
	Sysutils,
	Strutils,
	Math,
	RegExpr,
	Classes;

type
	TNumberedSection = record
		Location : String;
		Section : String;
		Page : Integer;
	end;

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

	NumberedSection : Array of TNumberedSection;

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
	TocMaxLen : Integer;
	TocStr : String;
	TocEntry : String;
	TocMaxPageLen : Integer;

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
	if not(Src.DocumentElement.NodeName = 'Specification') then
	begin
		WriteLn(StdErr, 'ERR Root element must be <Specification>');
		Inc(Validate);
	end;

	MetaNode := Src.DocumentElement.FindNode('Meta');
	if not(Assigned(MetaNode)) then
	begin
		WriteLn(StdErr, 'ERR <Meta> missing');
		Inc(Validate);
	end
	else
	begin
		DateNode := MetaNode.FindNode('Date');
		if not(Assigned(DateNode)) then
		begin
			WriteLn(StdErr, 'ERR <Meta>.<Date> missing');
			Inc(Validate);
		end;

		TitleNode := MetaNode.FindNode('Title');
		if not(Assigned(TitleNode)) then
		begin
			WriteLn(StdErr, 'ERR <Meta>.<Title> missing');
			Inc(Validate);
		end;

		ShortNode := MetaNode.FindNode('Short');
		if not(Assigned(ShortNode)) then
		begin
			WriteLn(StdErr, 'ERR <Meta>.<Short> missing');
			Inc(Validate);
		end;

		MetaOrgNode := MetaNode.FindNode('Organization');
		if not(Assigned(MetaOrgNode)) then
		begin
			WriteLn(StdErr, 'ERR <Meta>.<Organization> missing');
			Inc(Validate);
		end;

		AuthorsNode := MetaNode.FindNode('Authors');
		if not(Assigned(AuthorsNode)) then
		begin
			WriteLn(StdErr, 'ERR <Meta>.<Authors> missing');
			Inc(Validate);
		end;
	end;

	Child := Src.DocumentElement.FirstChild;
	while Assigned(Child) do
	begin
		if Child.NodeName = 'Author' then
		begin
			if not(Assigned(TDOMElement(Child).FindNode('Name'))) then
			begin
				WriteLn(StdErr, 'ERR <Author>.<Name> missing');
				Inc(Validate);
			end;
			if not(Assigned(TDOMElement(Child).FindNode('Organization'))) then
			begin
				WriteLn(StdErr, 'ERR <Author>.<Organization> missing');
				Inc(Validate);
			end;
		end;
		Child := Child.NextSibling;
	end;
end;

procedure Section(Node : TDOMNode; Numbered : Boolean; InitialIndent : Integer; Nest : Integer; DoList : Boolean; DoCenter : Boolean);
var
	Indent : Integer;
	R : TStringList;
	Lines : TStringList;
	I : Integer;
	J : Integer;
	B : Boolean;
	S : String;
	FirstLine : Boolean;
	IncrAmount : Integer;
	Col : String;
begin
	FirstLine := True;

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
		if DoCenter then
		begin
			OutputStr(StringOfChar(' ', Floor(PaperWidth / 2 - Length(Lines[I]) / 2)) + Lines[I]);
			continue;
		end;
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
				Col := StringOfChar(' ', Indent);
				if FirstLine and DoList then
				begin
					FirstLine := False;
					Col := StringOfChar(' ', Indent - PaddingLeft) + 'o' + StringOfChar(' ', PaddingLeft - 1);
				end;
				OutputStr(Col + S);
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
		Col := StringOfChar(' ', Indent);
		if FirstLine and DoList then
		begin
			FirstLine := False;
			Col := StringOfChar(' ', Indent - PaddingLeft) + 'o' + StringOfChar(' ', PaddingLeft - 1);
		end;
		OutputStr(Col + S);
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

	if Numbered and TDOMElement(Node).HasAttribute('Name') then
	begin
		Inc(SectionN[Nest]);
		for I := 1 to Nest do
		begin
			SectionNumber := SectionNumber + IntToStr(SectionN[I]) + '.';
		end;
		SetLength(NumberedSection, Length(NumberedSection) + 1);
		NumberedSection[Length(NumberedSection) - 1].Location := SectionNumber;
		NumberedSection[Length(NumberedSection) - 1].Section := String(TDOMElement(Node).GetAttribute('Name'));
		NumberedSection[Length(NumberedSection) - 1].Page := PageN + 1;

		SectionNumber := SectionNumber + ' ';
	end;

	if TDOMElement(Node).HasAttribute('Name') then
	begin
		if (PaperHeight - LineN) <= 5 then begin
			NewPage();
		end;

		OutputStr(SectionNumber + String(TDOMElement(Node).GetAttribute('Name')));
		OutputStr();
	end;
end;

procedure ScanSection(Node : TDOMNode; Numbered : Boolean; Indent : Integer; Nest : Integer);
var
	Child : TDOMNode;
	MoreIndent : Integer;
begin
	if (Node.NodeName = 'Section') or (Node.NodeName = 'IndentSection') or (Node.NodeName = 'List') or (Node.NodeName = 'Center') then
	begin
		MoreIndent := 0;
		if (Node.NodeName = 'IndentSection') or (Node.NodeName = 'List') then
		begin
			MoreIndent := PaddingLeft;
		end;
		Child := Node.FirstChild;
		SectionName(Node, Numbered, Indent + MoreIndent, Nest);
		while Assigned(Child) do
		begin
			if Child.NodeName = 'Content' then
			begin
				Section(Child, Numbered, Indent + MoreIndent, Nest, Node.NodeName = 'List', Node.NodeName = 'Center');
			end
			else
			begin
				ScanSection(Child, Numbered, Indent + MoreIndent, Nest + 1);
			end;
			Child := Child.NextSibling;
		end;
	end
	else if Node.NodeName = 'Numbered' then
	begin
		InitSectionNumber();
		ScanSections(Node, True, Indent, Nest);
	end
	else if Node.NodeName = 'TOC' then
	begin
		HaveToc := True;
	end
	else if Node.NodeName = 'NewPage' then
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

procedure Render(AuthorList : Boolean);
begin
	LineN := 0;
	PageN := 0;
	Buffer := '';
	TempNode := Src.DocumentElement.FirstChild;
	FirstTime := True;
	while Assigned(TempNode) do
	begin
		if TempNode.NodeName = 'Author' then
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
			NameNode := TempNode.FindNode('Name');
			OrgNode := TempNode.FindNode('Organization');
			OutputStr(AtStr + StringOfChar(' ', PaperWidth - Length(NameNode.TextContent) - Subtr) + String(NameNode.TextContent));
			OutputStr(AtStr2 + StringOfChar(' ', PaperWidth - Length(OrgNode.TextContent) - Subtr2) + String(OrgNode.TextContent));
		end;
		TempNode := TempNode.NextSibling;
	end;
	OutputStr(StringOfChar(' ', PaperWidth - Length(DateNode.TextContent)) + String(DateNode.TextContent));

	OutputStr();
	OutputStr(StringOfChar(' ', Floor(PaperWidth / 2 - Length(TitleNode.TextContent) / 2)) + String(TitleNode.TextContent));
	OutputStr();

	if AuthorList then
	begin
		TempNode := Src.CreateElement('NewPage');
		Src.DocumentElement.AppendChild(TempNode);
	
		TempNode := Src.CreateElement('Section');
		TempContentNode := Src.CreateElement('Content');
		TDOMElement(TempNode).SetAttribute('Name', 'Authors'' Addresses');
		AuthorNode := Src.DocumentElement.FirstChild;
		while Assigned(AuthorNode) do
		begin
			if AuthorNode.NodeName = 'Author' then
			begin
				NameNode := AuthorNode.FindNode('Name');
				OrgNode := AuthorNode.FindNode('Organization');
				EmailNode := AuthorNode.FindNode('Email');
	
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
	end;

	ScanSections(Src.DocumentElement, False, 0, 1);

	if not(LineN = 0) then
	begin
		NewPage(False);
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
	SetLength(NumberedSection, 0);
	Render(True);
	if HaveToc then
	begin

		TocNode := Src.DocumentElement.FindNode('TOC');
		
		TempNode := Src.CreateElement('Section');
		TempContentNode := Src.CreateElement('Content');

		TocMaxLen := 0;
		TocMaxPageLen := 0;
		for TocCount := 1 to Length(NumberedSection) do
		begin
			TocPrefix := StringOfChar('#', PaperWidth - 9 - Length(IntToStr(TocCount)) - PaddingLeft);
			TempContentNode.TextContent := UnicodeString(String(TempContentNode.TextContent) + TocPrefix + 'TOC-INDEX' + IntToStr(TocCount) + NL);
			if TocMaxLen < Length(NumberedSection[TocCount - 1].Location) then
			begin
				TocMaxLen := Length(NumberedSection[TocCount - 1].Location);
			end;
			if TocMaxPageLen < Length(IntToStr(NumberedSection[TocCount - 1].Page)) then
			begin
				TocMaxPageLen := Length(IntToStr(NumberedSection[TocCount - 1].Page));
			end;
		end;
		Inc(TocMaxLen);

		TDOMElement(TempNode).SetAttribute('Name', 'Table of Contents');
		TempNode.AppendChild(TempContentNode);

		Src.DocumentElement.InsertBefore(TempNode, TocNode);
		SetLength(NumberedSection, 0);
		Render(False);

		for TocCount := 1 to Length(NumberedSection) do
		begin
			TocRegExpr := TRegExpr.Create('#+TOC-INDEX' + IntToStr(TocCount));
			TocRegExpr.Exec(Buffer);

			TocEntry := NumberedSection[TocCount - 1].Location + StringOfChar(' ', TocMaxLen - Length(NumberedSection[TocCount - 1].Location)) + NumberedSection[TocCount - 1].Section;
			TocStr := TocEntry + ' ' + Copy(ReplaceStr(StringOfChar('.', Length(TocRegExpr.Match[0])), '..', '. '), Length(TocEntry) + 1, Length(TocRegExpr.Match[0]) - Length(TocEntry) - 1 - TocMaxPageLen - 1) + ' ' + StringOfChar(' ', TocMaxPageLen - Length(IntToStr(NumberedSection[TocCount - 1].Page))) +IntToStr(NumberedSection[TocCount - 1].Page);

			Buffer := TocRegExpr.Replace(Buffer, TocStr);

			TocRegExpr.Free();
		end;
	end;
	Write(Buffer);
	Src.Free();
end.
