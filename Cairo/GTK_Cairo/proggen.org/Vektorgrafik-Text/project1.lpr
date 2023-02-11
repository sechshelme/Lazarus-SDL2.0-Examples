program project1;

uses
  Cairo,
  glib2,
  gdk2,
  gtk2;

  function on_Exposs_Event(widget: PGtkWidget; event: PGdkEventExpose; Data: gpointer): gboolean;
  var
    cr: Pcairo_t;
  begin
    cr := gdk_cairo_create(widget^.window);

    cairo_select_font_face(cr, 'Courier', CAIRO_FONT_SLANT_ITALIC, CAIRO_FONT_WEIGHT_NORMAL);
    cairo_set_font_size(cr, 20.0);

    cairo_move_to(cr, 10, 20);
    cairo_show_text(cr, 'Hello World');

    cairo_select_font_face(cr, 'Courier', CAIRO_FONT_SLANT_OBLIQUE, CAIRO_FONT_WEIGHT_NORMAL);

    cairo_move_to(cr, 10, 40);
    cairo_show_text(cr, 'Hello World');

    cairo_stroke(cr);

    cairo_destroy(cr);
    Result := False;
  end;

  function main: integer;
  var
    window: PGtkWidget;
  begin
    gtk_init(@argc, @argv);
    window := gtk_window_new(GTK_WINDOW_TOPLEVEL);

    g_signal_connect(window, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
    g_signal_connect(window, 'expose-event', G_CALLBACK(@on_Exposs_Event), nil);

    gtk_widget_set_app_paintable(window, gTRUE);
    gtk_widget_show_all(window);

    gtk_main;
    Result := 0;
  end;

begin
  main;
end.
