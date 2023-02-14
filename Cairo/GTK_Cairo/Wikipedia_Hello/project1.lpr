program project1;

uses
  Cairo,
  glib2,
  gdk2,
  gtk2;

  function on_Exposs_Event(widget: PGtkWidget; event: PGdkEventExpose; Data: gpointer): gboolean;
  var
    cr: Pcairo_t;
    x, y: integer;
    pattern: Pcairo_pattern_t;
  begin
    cr := gdk_cairo_create(widget^.window);

    cairo_scale(cr, 6, 6);

    for x := 0 to 9 do begin
      for y := 0 to 9 do begin
        cairo_rectangle(cr, x * 10, y * 10, 5, 5);
      end;
    end;

    pattern := cairo_pattern_create_radial(50, 50, 5, 50, 50, 50);
    cairo_pattern_add_color_stop_rgb(pattern, 0, 0.75, 0.15, 0.99);
    cairo_pattern_add_color_stop_rgb(pattern, 0.9, 1, 1, 1);

    cairo_set_source(cr, pattern);
    cairo_fill(cr);

    cairo_set_font_size(cr, 15);
    cairo_select_font_face(cr, 'Georgia', CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD);
    cairo_set_source_rgb(cr, 0, 0, 0);

    cairo_move_to(cr, 25, 25);
    cairo_show_text(cr, 'Hello');

    cairo_move_to(cr, 20, 75);
    cairo_show_text(cr, 'World !');

    cairo_destroy(cr);
    Result := False;
  end;

  function main: integer;
  var
    window: PGtkWidget;
  begin
    gtk_init(@argc, @argv);
    window := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), 'Cairo-Demo');
    gtk_window_set_default_size(GTK_WINDOW(window), 570, 570);
    gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);


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
