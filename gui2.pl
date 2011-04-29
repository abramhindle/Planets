#!/usr/bin/perl
use strict;
# Use the TRUE and FALSE constants exported by the Glib module.
use Glib qw/TRUE FALSE/;
use Gtk2 '-init';
use threads;

# This is a callback function. We simply say hello to the world, and destroy
# the window object in order to close the program.
sub hello
{
	my ($widget, $window) = @_;
	print "Hello, World\n";

	$window->destroy;
}

sub delete_event
{
	# If you return FALSE in the "delete_event" signal handler,
	# GTK will emit the "destroy" signal. Returning TRUE means
	# you don't want the window to be destroyed.
	# This is useful for popping up 'are you sure you want to quit?'
	# type dialogs.
	print "delete event occurred\n";

	# Change TRUE to FALSE and the main window will be destroyed with
	# a "delete_event".
	return TRUE;
}

# create a new window
my $window = Gtk2::Window->new('toplevel');

# When the window is given the "delete_event" signal (this is given
# by the window manager, usually by the "close" option, or on the
# titlebar), we ask it to call the delete_event () functio
# as defined above. No data is passed to the callback function.
$window->signal_connect(delete_event => \&delete_event);

# Here we connect the "destroy" event to a signal handler.
# This event occurs when we call Gtk2::Widget::destroy on the window,
# or if we return FALSE in the "delete_event" callback. Perl supports
# anonymous subs, so we can use one of them for one line callbacks.
$window->signal_connect(destroy => sub { Gtk2->main_quit; });

# Sets the border width of the window.
$window->set_border_width(10);

# Creates a new button with a label "Hello World".
my $button = Gtk2::Button->new("Hello World");

# When the button receives the "clicked" signal, it will call the function
# hello() with the window reference passed to it.The hello() function is
# defined above.
$button->signal_connect(clicked => \&hello, $window);

# This packs the button into the window (a gtk container).
$window->add($button);
my $adjustment = Gtk2::Adjustment->new(0,
                                    0,
				    1000,
				    1,
				    1,
				    1);
my $scrollbar = Gtk2::HScrollBar->new(adjustment=>$adjustment);
$window->add($scrollbar);

# The final step is to display this newly created widget.
$button->show;
$scrollbar->show;

# and the window
$window->show;


sub ioloop {
	while(<>) {
		print "Got line: $_";
	}
}


my $ioloopthread = threads->create( 'ioloop' );


# All GTK applications must have a call to the main() method. Control ends here
# and waits for an event to occur (like a key press or a mouse event).
Gtk2->main;

$ioloopthread->join();

0;
