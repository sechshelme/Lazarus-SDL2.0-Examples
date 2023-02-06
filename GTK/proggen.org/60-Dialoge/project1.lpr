program project1;

uses
  Math,
  glib2,
  gdk2,
  gtk2;

const cmError=1000;

var
  Text: string;


  procedure show_dialog(button: PGtkButton; Data: gpointer);
  var
    error_msg: PGtkWidget;
  begin
    case integer(Data) of
      cmError: begin
        error_msg := gtk_message_dialog_new(nil, GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, 'Es ist ein Fehler aufgetreten !');
        gtk_dialog_run(GTK_DIALOG(error_msg));
        gtk_widget_destroy(error_msg);
      end;
    end;
  end;


  procedure change_text(button: PGtkButton; Data: gpointer);
  begin
    Text := gtk_entry_get_text(Data);
  end;

  procedure new_text(button: PGtkButton; Data: gpointer);
  begin
    gtk_entry_set_text(Data, 'Hello World !');
  end;


  function main(argc: integer; argv: PChar): integer;
  var
    window, vbox, label1, button1, button3, button2: PGtkWidget;
  begin
    gtk_init(@argc, @argv);
    window := gtk_window_new(GTK_WINDOW_TOPLEVEL);

    vbox := gtk_vbox_new(False, 0);
    gtk_container_add(GTK_CONTAINER(window), vbox);

    label1 := gtk_label_new('Gib etwas ein!');
    gtk_box_pack_start(GTK_BOX(vbox), label1, False, False, 0);


    button1 := gtk_button_new_with_label('Eingabe');
    gtk_box_pack_start(GTK_BOX(vbox), button1, False, False, 0);
    g_signal_connect(button1, 'clicked', G_CALLBACK(@show_dialog), Pointer(cmError));

    //button2 := gtk_button_new_with_label('Text Reset');
    //gtk_box_pack_start(GTK_BOX(vbox), button2, False, False, 0);
    //g_signal_connect(button2, 'clicked', G_CALLBACK(@new_text), textentry1);

    button3 := gtk_button_new_with_label('Programm beenden');
    gtk_box_pack_start(GTK_BOX(vbox), button3, False, False, 0);
    g_signal_connect(button3, 'clicked', G_CALLBACK(@gtk_main_quit), nil);


    g_signal_connect(window, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

    gtk_widget_show_all(window);

    gtk_main;
    Result := 0;
  end;

begin
  SetExceptionMask([exDenormalized, exInvalidOp, exOverflow, exPrecision, exUnderflow, exZeroDivide]);
  Halt(main(argc, @argv));
end.
