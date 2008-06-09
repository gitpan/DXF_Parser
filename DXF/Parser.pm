# DXF::Parser
#
# Copyright 2007 Charles Williams <chas@cmf.nrl.navy.mil>
# All rights reserved.
#
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

package DXF::Parser;

use strict;
use warnings;

BEGIN {
	use Exporter();
	our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

	$VERSION = "1.0";

	@ISA = qw(Exporter);
	@EXPORT = qw($name);
	%EXPORT_TAGS = (); 
	
	@EXPORT_OK = qw($name);
}

our $verbose;
our $debug;
our $disable_text;

sub new
{
	my ($class, %options) = @_;

	my $self = {};
	bless $self, $class;

	$verbose = $options{verbose};
	$debug = $options{debug};
	$disable_text = $options{disable_text};

	our $arc_segments = 10;		# @@@ - move this somewhere else

	return $self;
}

# counts for various items
my $dxf_npolyline = 0;
my $dxf_npolyline_gmesh = 0;
my $dxf_npolyline_fmesh = 0;
my $polyline_thickness;
my $polyline_type;
my $polyline_m;
my $polyline_n;
my $is_polyline;
my $polyline_3d;
my $polyline_closed;
my $polyline_Mclosed;
my $polyline_Nclosed;
my $dxf_nline = 0;
my $dxf_n3dline = 0;
my $dxf_nvertex = 0;
my $dxf_npoint = 0;
my $dxf_n3dface = 0;
my $dxf_ntext = 0;
my $dxf_nsolid = 0;
my $dxf_ntrace = 0;
my $dxf_ncircle = 0;
my $dxf_narc = 0;
my $dxf_ninsert = 0;
my $dxf_nblock = 0;
my $__entity_line;
my $output;

our @x;
our @y;
our @z;
our $extrude_x;
our $extrude_y;
our $extrude_z;
our @angle;
our @integer;
our @float;
our $layer;
our $thickness;
our $elevation;
our $variable;
our $ltype;
our $desc;
our $string4;
our $name;
our @v_x;
our @v_y;
our @v_z;
our $v_n;
our $text;
our %ltype;

sub
parse_from_file
{
	my ($self, $dxf) = @_;

	my $lastentity = "none";
	my $group;
	my $value;
	my $entity;
	my $in_section;
	my $in_table;
	my $section_name;
	my $table_name;
	my $__line__ = 0;
	my $color;
	my $follows;
	my $textstyle;
	my $repeated;
	my $handle;
	my $font;
	my $seen_elevation = 0;

	open(DXF,"<$dxf") || do {
		$dxf = "$dxf.dxf"; # try appending .dxf
		open(DXF,"<$dxf") || do {
			print STDERR "dxftoobj: failed to open $dxf\n";
			exit;
		};
	};

	($output = $dxf) =~ s/\.dxf$//;

	$self->initialize;

	while(<DXF>) {
		# group followed by value...
		$group = $_; ++$__line__;
		$group =~ s/[\012\015]+$//;	# chop doesn't remove ^M

		$value = <DXF>; ++$__line__;
		$value =~ s/[\012\015]+$//;

		#
		# basically, discovering and decoding entities are
		# the major part of decoding a dxf file
		#
		$group == 0 && do {
			defined $entity && ( $lastentity = $entity );

			case:
			{
				# have any entities been seen yet?
				defined $lastentity || last case;

				# process the last entity
				$lastentity eq "VERTEX" &&   ($self->dxf_vertex, last case);
				$lastentity eq "3DFACE" &&   ($self->dxf_3dface, last case);
				$lastentity eq "LINE" &&     ($self->dxf_line, last case);
				$lastentity eq "3DLINE" &&   ($self->dxf_3dline, last case);
				$lastentity eq "POLYLINE" && ($self->dxf_polyline, last case);
				$lastentity eq "SEQEND" &&   ($self->dxf_seqend, last case);
				$lastentity eq "ARC" &&      ($self->dxf_arc, last case);
				$lastentity eq "CIRCLE" &&   ($self->dxf_circle, last case);
				$lastentity eq "SOLID" &&    ($self->dxf_solid, last case);
				$lastentity eq "TRACE" &&    ($self->dxf_trace, last case);
				$lastentity eq "POINT" &&    ($self->dxf_point, last case);
				$lastentity eq "TEXT" &&     ($self->dxf_text, last case);
			
				$lastentity eq "BLOCK" &&    ($self->dxf_block, last case);
				$lastentity eq "ENDBLK" &&   ($self->dxf_endblk, last case);
				$lastentity eq "INSERT" &&   ($self->dxf_insert, last case);

				$lastentity eq "LTYPE" &&    ($self->dxf_ltype, last case);

				# ignore these entities for now
				$lastentity eq "VPORT" && (last case);
				$lastentity eq "LAYER" && (last case);
				$lastentity eq "STYLE" && (last case);
				$lastentity eq "UCS" && (last case);
				$lastentity eq "VIEW" && (last case);
				$lastentity eq "ATTDEF" && (last case);
			
				# these are special and handled elsewhere...
				$lastentity eq "SECTION" && (last case);
				$lastentity eq "ENDSEC" && (last case);
				$lastentity eq "TABLE" && (last case);
				$lastentity eq "ENDTAB" && (last case);

				$lastentity ne "none" &&
					$self->info("DXF: unsupported ENTITY \"$lastentity\"");
			};

			# now that we have processed the old entity 
			# reset the variables to the default values
			# (this isn't complete by any means at the moment)
			
			undef @integer;
			undef @float;
			undef $layer;
			$thickness = 0.0;
			undef $elevation;
			$angle[0] = 0.0;

			$integer[0] = 0;

			$x[0] = 0.0; $x[1] = 0.0; $x[2] = 0.0; $x[3] = 0.0; $x[4] = 0.0;
			$y[0] = 0.0; $y[1] = 0.0; $y[2] = 0.0; $y[3] = 0.0; $y[4] = 0.0;
			$z[0] = 0.0; $z[1] = 0.0; $z[2] = 0.0; $z[3] = 0.0; $z[4] = 0.0;

			$extrude_x = 0;	# default values for extrusions
			$extrude_y = 0;
			$extrude_z = 1;

			$entity = $value; # current entity
			$__entity_line = $__line__;

			$entity eq "SECTION" && do {
				$in_section = 1;
				$section_name = "";
			};

			$entity eq "TABLE" && do {
				$in_table = 1;
				$table_name = "";
			};

			$entity eq "ENDSEC" && ($in_section = 0);
			$entity eq "ENDTAB" && ($in_table = 0);

			$entity eq "EOF" && ( $self->dxf_eof, exit 0);

			next;
		};

		# handle all the other various group codes by setting some global
		# variables used throughout the dxf_* routines

		$group == 1 && do { $text = $value; next; };

		$group == 2 && do {
			$name = $value;

			if ( $entity eq "SECTION" && $section_name eq "" ) {
				$section_name = $name;
				$verbose && $self->info("dxftoobj: processing SECTION \"$section_name\"");
				#if ( $section_name eq "ENTITIES" )
				#	{ $self->initialize; }
			}

			if ( $entity eq "TABLE" && $table_name eq "" ) {
				$table_name = $name;
			}

			next;
		};

		$group == 3 && do { $desc = $value; next };
		$group == 4 && do { $string4 = $value; next };

		$group == 5 && do { $handle = $value; next };

		$group == 6 && do { $ltype = $value; next };
		$group == 7 && do { $textstyle = $value; next };
		$group == 8 && do { $layer = $value; next };
		$group == 9 && do {
			# process the last variable "group"
			defined $variable && $self->variable;
			$variable = $value;
			next;
		};

		$group == 39 && do { $thickness = $value; next };

		$group == 49 && do { $repeated = $value; next };

		$group == 50 && do { $angle[0] = $value; next };
		$group == 51 && do { $angle[1] = $value; next };

		$group == 62 && do { $color = $value; next };

		$group == 66 && do { $follows = $value; next };

		# 10-18 - x[0] through x[8]
		$group > 9 && $group < 19 &&
			do{ $x[$group-10] = $value; next };

		# 20-28 - y[0] through y[8]
		$group > 19 && $group < 29 &&
			do{ $y[$group-20] = $value; next };

		# 30-38 - z[0] through z[8]
		$group > 29 && $group < 38 &&
			do{ $z[$group-30] = $value; next };

		$group == 38 && do {
			if ( ! $seen_elevation ) {
				$seen_elevation = 1;
				$self->warn("DXF: obselete elevation code in use");
			}

			$elevation = $value;
			next;
		};

		# 40-48 - float[0] through float[8]
		$group > 39 && $group < 49 &&
			do { $float[$group-40] = $value; next };

		$group > 69 && $group < 80 &&
			do { $integer[$group-70] = int($value); next };

		$group == 210 && do { $extrude_x = $value; next };
		$group == 220 && do { $extrude_y = $value; next };
		$group == 230 && do { $extrude_z = $value; next };

		# Canvas 3.x uses 999 to encode font names
		$group == 999 && do { $font = $value; next };

		$group == 100 && next;	# XXX ignore group 100, what the fuck is it?

		$self->debug("DXF: unsupported group code: $group=\"$value\"");
	}

	# we shouldn't exit here

	$self->warn("dxftoobj: EOF group not seen.  File possibly truncated.");
	$self->dxf_eof;

}

sub
warn
{
	my ($self, $mesg) = @_;
	printf STDERR "$mesg\n";
}

sub
info
{
	my ($self, $mesg) = @_;
	printf STDOUT "$mesg\n";
}

sub
debug
{
	my ($self, $mesg) = @_;
	$debug && printf STDERR "$mesg\n";
}


# dxf_init -- prepare to output dxf entities	@@@ misnamed
#
sub
initialize
{
	my $self = shift;

	$self->open($output);
	$self->comment("DXF::Parser 1.0");		# $VERSION
}

sub
variable
{
	my $self = shift;

	my $dxf_minext_x;
	my $dxf_minext_y;
	my $dxf_minext_z;
	my $dxf_maxext_x;
	my $dxf_maxext_y;
	my $dxf_maxext_z;

	$variable eq "\$EXTMIN" && do {
		$dxf_minext_x = $x[0];
		$dxf_minext_y = $y[0];
		$dxf_minext_z = $z[0];
	};

	$variable eq "\$EXTMAX" && do {
		$dxf_maxext_x = $x[0];
		$dxf_maxext_y = $y[0];
		$dxf_maxext_z = $z[0];
	};
}

sub
dxf_ltype
{
	my $self = shift;

	#printf "ltype %s desc %s\n", $name, $desc;

	$ltype{$name} = {
		flag => $integer[0],
		desc => $desc,
	};
}

# dxf_circle -- draw an arc about the center $x[0], $y[0], $z[0] with radius
#              $float[0]
#
# BUGS
#   doesn't handle the case where the arc is not on the XY plane
#
sub
dxf_circle
{
	my $self = shift;

	++$dxf_ncircle;

	defined $elevation && $self->warn("CIRCLE: elevation unsupported");
	
	$debug && $self->comment("CIRCLE $x[0] $y[0] $z[0] $float[0]");

	my $object = {
		x => $x[0],
		y => $y[0],
		z => $z[0],
		radius => $float[0],
		layer => $layer,
		thickness => $thickness,
		ltype => $ltype,
	};
		
	$self->circle($object);
}

# dxf_arc -- draw an arc about the center $x[0], $y[0], $z[0] with radius
#            $float[0], from $angle[0] to $angle[1].
# BUGS
#   Doesn't understand the case where the arc is not on the XY plane
#
sub
dxf_arc
{
	my $self = shift;

	++$dxf_narc;

	defined $elevation && $self->warn("ARC: elevation unsupported");

	$debug && $self->comment("ARC $x[0] $y[0] $z[0] $float[0] $angle[0] $angle[1]");

	my $object = {
		x => $x[0],
		y => $y[0],
		z => $z[0],
		radius => $float[0],
		angle1 => $angle[0],
		angle2 => $angle[1],
		layer => $layer,
		thickness => $thickness,
		ltype => $ltype,
	};

	$self->arc($object);
}

# dxf_3dline -- draw a line from x[0], y[0], z[0] to x[1], y[1], z[1]
#
sub
dxf_3dline
{
	my $self = shift;

	++$dxf_n3dline;

	defined $elevation && $self->warn("3DLINE: elevation unsupported");

	$debug && $self->comment("3DLINE");

	my $object = {
		n => 2,
		closed => 0,
		x => \@x,
		y => \@y,
		z => \@z,
		layer => $layer,
		thickness => $thickness,
		ltype => $ltype,
	};

	$self->line($object);
}

# dxf_line -- draw a line from x[0], y[0], 0.0 to x[1], y[1], 0.0
#
sub
dxf_line
{
	my $self = shift;

	++$dxf_nline;

	defined $elevation && $self->warn("LINE: elevation unsupported");

	$debug && $self->comment("LINE");

	my $object = {
		n => 2,
		closed => 0,
		x => \@x,
		y => \@y,
		z => \@z,
		layer => $layer,
		thickness => $thickness,
		ltype => $ltype,
	};

	$self->line($object);
}

# dxf_trace -- draw a quadrilateral using x[0], y[0], z[0] through 
#              x[3], y[3], z[3]
sub
dxf_trace
{
	my $self = shift;
	my $i;

	++$dxf_ntrace;

	defined $elevation && do {
		for( $i=0; $i<4; ++$i)
			{ $z[$i] += $elevation; }
	};

	$debug && $self->comment("TRACE");

	my $object = {
		n => 4,
		x => \@x,
		y => \@y,
		z => \@z,
		layer => $layer,
		thickness => $thickness,
	};

	$self->polygon($object);
}

# solid -- draw a quadrilateral or triangle using x[0], y[0], z[0]
#          through x[3], y[3], z[3]  (the last two points are the
#          same if its a triangle)
#
sub
dxf_solid
{
	my $self = shift;
	my $i;

	++$dxf_nsolid;

	defined $elevation && do {
		for( $i=0; $i<4; ++$i)
			{ $z[$i] += $elevation; }
	};

	$debug && $self->comment("SOLID");

	my $object = {
		n => 4,
		x => \@x,
		y => \@y,
		z => \@z,
		layer => $layer,
		thickness => $thickness,
	};

	if ($x[2]==$x[3] && $y[2]==$y[3] && $z[2]==$z[3]) {
		# a triangle
		$object = {
			n => 3,
			x => \@x,
			y => \@y,
			z => \@z,
			layer => $layer,
			thickness => $thickness,
		}
	}

	$self->polygon($object);
}

# dxf_3dface -- draw a quadrilateral or triangle using x[0], y[0], z[0]
#               through x[3], y[3], z[3]  (the last two points are the
#               same if its a triangle)
# BUGS
#   seems very similar to solid, what is the difference
#
sub
dxf_3dface
{
	my $self = shift;

	++$dxf_n3dface;

	$debug && $self->comment("3DFACE");

	my $object = {
		n => 4,
		x => \@x,
		y => \@y,
		z => \@z,
		layer => $layer,
	};

	if ($x[2]==$x[3] && $y[2]==$y[3] && $z[2]==$z[3]) {
		# a triangle
		$object = {
			n => 3,
			x => \@x,
			y => \@y,
			z => \@z,
		};
	}
	$self->polygon($object);
}

# dxf_point -- draw a point at x[0], y[0], z[0]
#
# BUGS
#   not implemented -- unsure how useful this is
#
sub
dxf_point
{
	my $self = shift;

	++$dxf_npoint;

	defined $elevation && $self->warn("POINT: elevation unsupported");

	$self->debug("POINT: ignoring POINT entity");
}

# dxf_text -- place a string of text at location x[0], y[0], z[0]
#
sub
dxf_text
{
	my $self = shift;

	++$dxf_ntext;

	return if $disable_text;

	$debug && $self->comment("TEXT $text");

	my $object = {
		text => $text,
		x => $x[0],
		y => $y[0],
		z => $z[0],
		scale => $float[0],
		layer => $layer,
		angle => $angle[0],
	};

	$self->text($object);
}


# flag -- check to see if $flag is set in $value
#
sub
flag
{
	my ($value, $flag) = @_;
	return (($value & $flag) == $flag);
}

# dxf_polyline -- one of the following:
#
#   line           list of vertices defining points along a path
#   polyface mesh  list of vertices defining points and faces
#   polygon mesh   list of vertices describing a rectangular mesh
#
# BUGS
#   meshes are always closed in M/N direction
#   smoothing points not supported
#   fitting points for mesh (group 75) are not supported
#   invisible edges on polygon mesh are ignored
#
sub
dxf_polyline
{
	my $self = shift;

	++$dxf_npolyline;

	$polyline_thickness = $thickness;

	# flags for polyline group
	#   1 - closed (or closed in M direction for mesh)
	#   2 - has curve fitting points
	#   4 -
	#   8 - 3D polyline (if we make this polyline a face, this matters)
	#  16 - polygon mesh
	#  32 - closed in N direction
	#  64 - polyface mesh
	# 128 - linetype

	$polyline_type = "POLYLINE OPEN";
	$is_polyline = 1;
	$polyline_closed = 0;
	$polyline_3d = 0;	# ignored
	$polyline_Mclosed = 0;	# closed in "m" direction (unsupported)
	$polyline_Nclosed = 0; 	# closed in "n" direction (unsupported)

	$v_n = 0;

	&flag($integer[0],1) && do {
		$integer[0] -= 1;

		$polyline_type = "POLYLINE CLOSED";
		$polyline_closed = 1;
		$polyline_Mclosed = 1;
	};

	&flag($integer[0],2) && do {
		$integer[0] -= 2;
		$self->warn("POLYLINE: curve fitting points ignored");
	};

	&flag($integer[0],8) && do {
		$integer[0] -= 8;
		$polyline_3d = 1;
	};

	&flag($integer[0],32) && do {
		$integer[0] -= 32;

		$polyline_Nclosed = 1;
	};

	&flag($integer[0],128) && do {
		$self->warn("POLYLINE: ignoring linetype");
		$integer[0] -= 128;
	};

	# polyline is a polyface mesh

	&flag($integer[0],64) && do {
		$integer[0] -= 64;

		++$dxf_npolyline_fmesh;
		$polyline_type = sprintf("POLYFACE MESH (%dx%d)", $integer[2], $integer[1]); 
	};

	# polyline is a polygon mesh

	&flag($integer[0],16) && do {
		$integer[0] -= 16;

		++$dxf_npolyline_gmesh;

		$polyline_m = $integer[1];
		$polyline_n = $integer[2];
		$polyline_type = sprintf("POLYGON MESH (%dx%d)", $polyline_m, $polyline_n);
	};

	$integer[0] != 0 && do {
		$self->warn("POLYLINE: unsupported bit codes in group 70 (flags: $integer[0], starting line: $__entity_line)");
	};

	$debug && $self->comment("$polyline_type");
}

# dxf_seqend -- mark the end of a vertex list of a polyline entity
#               or end the end of an attribute entry
#
sub
dxf_seqend
{
	my $self = shift;

	my $i;
	my $j;

	# we only handle "plain" polyline here -- meshes are handled in
	# dxf_vertex

	if ( $is_polyline == 1 ) {
		# POLYLINE OPEN/CLOSED

		($polyline_type =~ /^POLYLINE/) && do {

			my $object = {
				n => $v_n,
				closed => $polyline_closed,
				x => \@v_x,
				y => \@v_y,
				z => \@v_z,
				layer => $layer,
				thickness => $polyline_thickness,
				ltype => $ltype,
			};

			$self->line($object);
		};

		# POLYGON MESH

		($polyline_type =~ /^POLYGON MESH/) && do {

			for($i = 0; $i < ($polyline_m * $polyline_n); $i += $polyline_n) {	# m-2 * n-2 i suspect
				for($j = 0; $j < ($polyline_n - 1); ++$j) {		# -2 probably


					#   $polyline_m - 1
					#   ...
					#   0 ... $polyline_n - 1

					my $p1 = $i + $j;
					my $p2 = $p1 + $polyline_n;
					my $p3 = $p2 + 1;
					my $p4 = $p1 + 1;

					my $object = {
						n => 4,
						x => [$v_x[$p1], $v_x[$p2], $v_x[$p3], $v_x[$p4]],
						y => [$v_y[$p1], $v_y[$p2], $v_y[$p3], $v_y[$p4]],
						z => [$v_z[$p1], $v_z[$p2], $v_z[$p3], $v_z[$p4]],
						layer => $layer,
					};

					$self->polygon($object);
				}
			}

			# FIXME -- handle closed in M or N directions
		};

		# POLYFACE MESH

		# see dxf_vertex()

		$is_polyline = 0;
	}
}

# dxf_vertex --
#
# 
sub
dxf_vertex
{
	my $self = shift;

	++$dxf_nvertex;

	defined $elevation && ($z[0] += $elevation);

	# POLYFACE MESH

	&flag($integer[0], 128) && do {
		if (&flag($integer[0], 64)) {

			# a vertex that will be used later
			# in defining a face on the mesh

			$v_x[$v_n] = $x[0];
			$v_y[$v_n] = $y[0];
			$v_z[$v_n] = $z[0];
			++$v_n;
		} else {

			# FACE
			
			# negative index values mean the edge is invisible
			# (currently unsupported and ignored)

			$integer[1] = abs($integer[1]);
			$integer[2] = abs($integer[2]);
			$integer[3] = abs($integer[3]);

			# this isnt very efficient since it will make copies
			# copies of shared vertices.  perhaps there a mesh
			# specific output routine

			my $object = {
				n => 3,
				x => [$v_x[$integer[1] - 1], $v_x[$integer[2] - 1], $v_x[$integer[3] - 1]],
				y => [$v_y[$integer[1] - 1], $v_y[$integer[2] - 1], $v_y[$integer[3] - 1]],
				z => [$v_z[$integer[1] - 1], $v_z[$integer[2] - 1], $v_z[$integer[3] - 1]],
				layer => $layer,
				thickness => $thickness,	# maybe polyline_thickness
			};

			$self->polygon($object);
		}

		return;
	};

	# a vertex on a polygon mesh
	&flag($integer[0], 64) && do {
		$v_x[$v_n] = $x[0];
		$v_y[$v_n] = $y[0];
		$v_z[$v_n] = $z[0];
		++$v_n;

		return;
	};

	# a vertex on a 3D polyline
	&flag($integer[0], 32) && do {
		$v_x[$v_n] = $x[0];
		$v_y[$v_n] = $y[0];
		$v_z[$v_n] = $z[0];
		++$v_n;

		return;
	};

	# ???
	($integer[0] == 0) && do {
		$v_x[$v_n] = $x[0];
		$v_y[$v_n] = $y[0];
		$v_z[$v_n] = $z[0];
		++$v_n;

		return;
	};

	if ( $integer[0] != 0 )
		{ $self->debug("VERTEX: unsupported flags $integer[0]"); }
}

# block -- defines a group of entities that may be INSERT'ed later
# BUGS
#   currently redefines output file to store groups as seperate files
#     that can later be read
#   doesn't save OUT (although this is possible but doesn't matter since
#     OUT isn't open when BLOCK's are defined)
#
sub
dxf_block
{
	my $self = shift;

	++$dxf_nblock;

	$debug && $self->comment("BLOCK $name");
}

# endblk -- ends a BLOCK
#
sub
dxf_endblk
{
	my $self = shift;

	$debug && $self->comment("ENDBLK $name");
}

# insert -- insert a BLOCK group, scaling by float[1], float[2], float[3]
#           and translating by x[0], y[0], and z[0]
# BUGS
#   BLOCK/ENDBLK sequence must occur before insert so group is available
#
sub
dxf_insert
{
	my $self = shift;

	++$dxf_ninsert;

	$debug && $self->comment("INSERT $name");

	# if the scale for inserted entity is not specified, use 1,1,1
	!defined $float[1] && ($float[1] = 1.0);
	!defined $float[2] && ($float[2] = 1.0);
	!defined $float[3] && ($float[3] = 1.0);
}


# dxf_eof -- print statistics and close DXF file
#
sub
dxf_eof
{
	my $self = shift;

	if ($verbose) {
		$self->info(".dxf statistics:");
		if ($dxf_npolyline) {
			$self->info("\t$dxf_npolyline POLYLINE entities");
			$self->info("\t\t$dxf_npolyline_gmesh POLYGON MESH entities");
			$self->info("\t\t$dxf_npolyline_fmesh POLYFACE MESH entities");
		}
		if ($dxf_nline) {
			$self->info("\t$dxf_nline LINE entities");
		}
		if ($dxf_n3dline)
			{ $self->info("\t$dxf_n3dline 3DLINE entities"); }
		if ($dxf_n3dface)
			{ $self->info("\t$dxf_n3dface 3DFACE entities"); }
		if ($dxf_nvertex)
			{ $self->info("\t$dxf_nvertex VERTEX entities"); }
		if ($dxf_npoint)
			{ $self->info("\t$dxf_npoint POINT entities"); }
		if ($dxf_ntext)
			{ $self->info("\t$dxf_ntext TEXT entities"); }
		if ($dxf_nsolid)
			{ $self->info("\t$dxf_nsolid SOLID entities"); }
		if ($dxf_nsolid)
			{ $self->info("\t$dxf_ncircle CIRCLE entities"); }
		if ($dxf_ncircle)
			{ $self->info("\t$dxf_narc ARC entities"); }
		if ($dxf_narc)
			{ $self->info("\t$dxf_ntrace TRACE entities"); }
		if ($dxf_nblock)
			{ $self->info("\t$dxf_nblock BLOCK entities"); }
		if ($dxf_ninsert)
			{ $self->info("\t$dxf_ninsert INSERT entities"); }
		$self->info("");
	}

	$self->close();
}

1;

__END__

=head1 NAME

DXF::Parser - A perl module for parsing DXF files

=head1 SYNOPSIS

 package DXF::<format>;

 use DXF::Parser;

 sub
 open { }

 sub
 polyline { }

 sub
 close { }
 
 $p = DXF::<format>->new( verbose => 1,
			debug => 0,
			disable_text => 0 );

 $p->parse_from_file( 'sample.dxf' );

=head1 DESCRIPTION

This module provides a way to parse DXF files.  You are expected
to inherit your output class from module and optionally implement
the following methods:  initialize, open, close, line, arc, circle,
text and polygon.

Additionally, you should implement dxf_block and dxf_insert.

=head1 METHODS

=over 4

=item initialize

Called by the parser to intialize the output class.  It should
call the super class initialize function.

=item open

Called by the parser to open the new output file.  It is passed
a filename prefix.

=item close

Called by the parser to close the output file.

=item line

Called by the parser to draw a line consisting of two
or more points.

=item arc

Called by the parser to draw an arc.

=item circle

Called by the parser to draw a circle.

=item text

Called by the parser to draw text.

=item polygon

Called by the parser to draw a polygon of three or more points.

=item dxf_block

This method (along with dxf_endblk and dxf_insert) implement the
BLOCK/INSERT function of DXF.  This method is called at the start of a
BLOCK section.

=item dxf_endblk

This method is called at the end of a BLOCK section.

=item dxf_insert

This method is called when a BLOCK is inserted in the current output file.
Typically, the dxf_block method has stored the requested BLOCK somewhere.

=back

=head1 EXAMPLES

see dxftoobj and dxftofig.

=head1 HISTORY

based on:

dxftoobj -- convert autocad .dxf files to wavefront's .obj format

=head1 TODO

Fix dxf_block/dxf_insert.  This should be handled entirely in the parser
module by keeping the BLOCKs as list of entitites.

Support rotatations (especially in INSERT blocks)

=head1 SEE ALSO

L<dxftoobj>, L<dxftofig>

=head1 COPYRIGHT

Copyright 2007 Charles Williams <chas@cmf.nrl.navy.mil>

This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut

