program project1;
const
  gtklib = 'libgtk-x11-2.0.so';
  //procedure gtk_dialog_add_buttons(dialog:Pointer; first_button_text:Pchar; args:array of const); overload; cdecl; external gtklib;
  procedure gtk_dialog_add_buttons(dialog: Pointer; first_button_text: PChar); cdecl; overload; varargs; external gtklib;
begin
  gtk_dialog_add_buttons(nil, 'Beenden', 100, 'Abbrechen', 101, 'Speichern', 102, nil);
end.
