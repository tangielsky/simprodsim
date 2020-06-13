unit Preferences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Spin, EditBtn;

type

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ColorButtonDesktop: TColorButton;
    ColorButtonWaitB: TColorButton;
    ColorButtonSetupB: TColorButton;
    ColorButtonProcessB: TColorButton;
    ColorButtonProcessF: TColorButton;
    ColorButtonSetupF: TColorButton;
    ColorButtonWaitF: TColorButton;
    EditOdbcName: TEdit;
    FileNameEditBackground: TFileNameEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SpinEditInterval: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
  private

  public

  end;

var
  PreferencesForm: TPreferencesForm;

implementation

{$R *.lfm}

end.

