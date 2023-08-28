unit Options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    CheckBoxFullScreen: TCheckBox;
    CheckBoxNoConfirm: TCheckBox;
  private

  public

  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

end.

