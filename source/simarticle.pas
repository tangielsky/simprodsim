unit SimArticle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


type
  TArticle = class
    private
      FId : string;
      FCaption : string;
      FImagePath : string;
    published
      property Id : string read FId write FId;
      property Caption : string read FCaption write FCaption;
      property ImagePath : string read FImagePath write FImagePath;
  end;


  TArticleList = class(TList)
    private
    public
      constructor Create;
      destructor Destroy;
      function FindById(ArticleId : string) : TArticle;
      procedure Clear; override;
    published
  end;



var
  ArticleList : TArticleList;

implementation


{ TArticleList }

constructor TArticleList.Create;
begin
  inherited Create;

end;

destructor TArticleList.Destroy;
begin

  inherited Destroy;
end;

function TArticleList.FindById(ArticleId: string): TArticle;
var
  i : integer;
  Article : TArticle;
begin
  result:=nil;
  for i:=0 to self.Count-1 do
    begin
      Article:=TArticle(self[i]);
      if Article.Id=ArticleId then
        begin
          result:=Article;
          exit;
        end;
    end;
end;

procedure TArticleList.Clear;
var
  i : integer;
  Article : TArticle;
begin
  for i:=0 to self.Count-1 do
    begin
      Article:=TArticle(self[i]);
      Article.Free;
    end;

  inherited Clear;
end;


  

initialization
  ArticleList:=TArticleList.Create;


finalization
  ArticleList.Free;



end.

