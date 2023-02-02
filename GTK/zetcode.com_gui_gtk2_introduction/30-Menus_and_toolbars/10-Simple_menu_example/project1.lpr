program project1;

uses
  glib2,
  gdk2,
  gtk2;

  procedure gtk_widget_set_tooltip_text(window: PGtkWidget; title: Pgchar); cdecl; external gtklib;

  procedure print_msg(widget: PGtkWidget; data: gpointer); cdecl;
  begin
    WriteLn('click');
  end;

  procedure menu_click_msg(widget: PGtkWidget; data: gpointer); cdecl;
  begin
    WriteLn('menu click');
    if data<>nil then WriteLn(PChar(data));
//    Write(PtrUInt(data));

  end;

  function main(argc: integer; argv: PChar): integer;
  var
    window, vbox, menubar, fileMi, quitMi, fileMenu, PrintMi, aboutMi, SubMi, SubMi0, SubMi1, SubMi2, subMenu: PGtkWidget;
  begin
    gtk_init(@argc, @argv);
    window := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), 'Simple menu');
    gtk_window_set_default_size(GTK_WINDOW(window), 300, 200);

    vbox := gtk_vbox_new(gFALSE, 0);
    gtk_container_add(GTK_CONTAINER(window), vbox);

    menubar := gtk_menu_bar_new;


    fileMenu := gtk_menu_new;
    fileMi := gtk_menu_item_new_with_mnemonic('_File');

    PrintMi := gtk_menu_item_new_with_mnemonic('_Print');
    g_signal_connect(G_OBJECT(PrintMi), 'activate', G_CALLBACK(@print_msg), nil);
    gtk_menu_shell_append(GTK_MENU_SHELL(fileMenu), PrintMi);

    subMenu := gtk_menu_new;
    SubMi0 := gtk_menu_item_new_with_mnemonic('Sub_0');
    gtk_menu_shell_append(GTK_MENU_SHELL(subMenu), SubMi0);
    g_signal_connect(G_OBJECT(SubMi0), 'activate', G_CALLBACK(@menu_click_msg), PChar('Sub 0...'));

    SubMi1 := gtk_menu_item_new_with_mnemonic('Sub_1');
    gtk_menu_shell_append(GTK_MENU_SHELL(subMenu), SubMi1);
    g_signal_connect(G_OBJECT(SubMi1), 'activate', G_CALLBACK(@menu_click_msg), PChar('Sub 1...'));

    SubMi2 := gtk_menu_item_new_with_mnemonic('Sub_2');
    gtk_menu_shell_append(GTK_MENU_SHELL(subMenu), SubMi2);
    g_signal_connect(G_OBJECT(SubMi2), 'activate', G_CALLBACK(@menu_click_msg), PChar('Sub 2...'));

    SubMi := gtk_menu_item_new_with_mnemonic('_Sub');

    gtk_menu_item_set_submenu(GTK_MENU_ITEM(SubMi), subMenu);
    gtk_menu_shell_append(GTK_MENU_SHELL(fileMenu), SubMi);

    quitMi := gtk_menu_item_new_with_mnemonic('_Quit');
    gtk_menu_shell_append(GTK_MENU_SHELL(fileMenu), quitMi);
    g_signal_connect(G_OBJECT(quitMi), 'activate', G_CALLBACK(@gtk_main_quit), nil);


    gtk_menu_shell_append(GTK_MENU_SHELL(menubar), fileMi);

    aboutMi := gtk_menu_item_new_with_mnemonic('_About...');
    gtk_menu_shell_append(GTK_MENU_SHELL(menubar), aboutMi);
    g_signal_connect(G_OBJECT(aboutMi), 'activate', G_CALLBACK(@menu_click_msg), PChar('About...'));

    gtk_menu_item_set_submenu(GTK_MENU_ITEM(fileMi), fileMenu);


    gtk_box_pack_start(GTK_BOX(vbox), menubar, gFALSE, False, 0);

    g_signal_connect(G_OBJECT(window), 'destroy', G_CALLBACK(@gtk_main_quit), nil);

    gtk_widget_show_all(window);
    gtk_main;
    Result := 0;
  end;

begin
  Halt(main(argc, @argv));
end.
