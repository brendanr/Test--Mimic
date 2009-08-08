package Test::Mimic::Recorder;

use 5.006001;   # For open( my $fh, ...
                # Note: The above line is currently lying. We actually need 5.10 for the
                # GetOptionsFromArray subroutine from Getopt::Long. We are eventually aiming
                # for compatibility with 5.006001.
use strict;
use warnings;

use Getopt::Long qw<GetOptionsFromArray>;
use Data::Dump::Streamer;

use Test::Mimic::Recorder::Scalar;
use Test::Mimic::Recorder::Array;
use Test::Mimic::Recorder::Hash;

our $VERSION = 0.001_001;
our $SuspendRecording = 0; # Turn off recording.
my  $save_to;
my  $done_writing = 0;

# Data to be stored.
my @references; # A table containing recorded data for volatile references and objects. The index of a
                # given reference is simply the number of references
                # Test::Mimic::Recorder::_Implementation::_watch saw before the reference under
                # consideration.
my %typeglobs;  # Contains recorded data for scalars, arrays, hashes and subroutines in a structure analogous
                # to the symbol table. The key is the package name.
my %extra;      # Currently contains only a flattened class hierarchy for each recorded class at
                # $extra{$class}{'ISA'} as a hash ref. $extra{$recorded_class}{'ISA'}{$other_class} will
                # exist iff $recorded_class isa $other_class.
my @operation_sequence; # An ordered list of recorded operations. The first operation happened first, the second
                        # operation happened second and so forth. This currently only includes subroutine calls in
                        # recorded packages. Orderings of various 'scopes' can later be extracted from this.

sub import {
    die "import should only be used as a method."
        if ( $_[0] ne 'Test::Mimic::Recorder' );

    if ( @_ == 1 ) { # We are using Test::Mimic::Recorder as a library, not to actively monitor a program.
        $done_writing = 1;
    }
    else {
        shift(@_);

        # Call _record_package on each package passing along the package and a list of scalars to record.
        my @scalars;
        GetOptionsFromArray(
            \@_, 'f=s' => \$save_to, 's=s' => \@scalars,
            '<>' => sub {
                @scalars = split( /,/, join( ',', @scalars ) );
                Test::Mimic::Recorder::_Implementation::_record_package( $_[0], @scalars );
            },
        ) or die "Bad options: $!";
        $save_to ||= 'test_mimic.rec';
    }
}

# Returns a structure representing the passed value suitable for stringification.
# See POD below for details.
# 
# Currently scalars et al., arrays, hashes, qr objects, code references are handled well.
# Filehandles are not being tied, ideally they would be, but the filehandle tying mechanism is
# not complete.
# Formats are in a similar position, but they probably shouldn't ever be redefined. (Check this.)
# Because of this that may not really be a problem.
# The entries in globs can not be tied. A special glob tie could potentially remedy this, but
# this does not currently exist.
sub wrap {
    my ( $records, $val, $is_volatile, $at_level ) = @_;
    my ( $references, $address_to_index, $alive ) = @{$records};
    
    return Test::Mimic::Recorder::_Implementation::_wrap_then_watch( $records, $val, $at_level );
}

# Given a wrapped element returns a string version. Should be suitable for use as a key in a hash as well as
# being invertible with destringify. Subclass Test::Mimic::Recorder to stringify with your preferred library.
#
# TODO: Will need to use SortKeys from Data::Dump::Streamer if we start allowing unblessed hashes to be
# wrapped. This is necessary because they may be used in an argument list and these need to match exactly
# from recording to playback. _wrap_then_watch currently will wrap hashes, but the only times we wrap as
# opposed to watch are in argument lists and return lists (and only for one 'level'), so this is not at
# present a problem.
sub stringify {
    my ($val) = @_;
    return scalar Dump($val)->Out();
}

# Writes recording to disk. Typically called automatically.
sub finish {
    $done_writing = 1; # Prevents the END block from overwriting what we just wrote.
    open( my $fh, '>', $save_to ) or die "Unable to open file: $!";
    print $fh Test::Mimic::Recorder::stringify( [ \@references, \%typeglobs, \%extra, \@operation_sequence ] )
        or die "Unable to write: $!";
    close($fh) or die "Unable to close file: $!";
}

package Test::Mimic::Recorder::_Implementation;

use Devel::EvalError ();
use Scalar::Util qw<blessed refaddr reftype weaken>;

use constant {
    # Array indices for the three contexts
    SCALAR_CONTEXT  => 0,
    LIST_CONTEXT    => 1,
    VOID_CONTEXT    => 2,
    
    # Reference types
    SCALAR      => 100, # SCALAR includes REF, LVALUE and VSTRING.
    ARRAY       => 101,
    HASH        => 102,
    CODE        => 103,
    IO          => 104,
    GLOB        => 105,
    FORMAT      => 106,
    REG_EXP     => 107,
    
    # Description of wrapped data
    STABLE      => 200,
    VOLATILE    => 201,
    OBJECT      => 202,
    NESTED      => 203,
    
    # The two types of supported behavior
    RETURN      => 300,
    EXCEPTION   => 301,
    
    # Convenience values
    ARBITRARY   => 400, # For merely creating hash entries
    
    # Event types
    CODE_E      => 500,
    SCALAR_E    => 501,
    ARRAY_E     => 502,
    HASH_E      => 503,
};

# Transient data. Will not be written to disk.
my %address_to_index;
my %alive;
my $records = [ \@references, \%address_to_index, \%alive ];

# Accepts a package name and a list of scalars in the package to be recorded. Test::Mimic::Recorder will
# begin monitoring this package including the passed scalars.
sub _record_package {
    my ( $package, @user_selected_scalars ) = @_; 
    
    eval("require $package; 1")
        or die "Failed to load package $package. $@";
    
    # Consider every symbol in the package, tie arrays and tie hashes.
    my $symbol_table;
    {
        no strict 'refs';
        $symbol_table = \%{ $package . '::' };
    }
    my $fake_package = ( $typeglobs{$package} ||= {} );
    for my $symbol ( keys %{$symbol_table} ) {
        my $typeglob = \$symbol_table->{$symbol};   # We need to take the reference here because my can not
                                                    # handle typeglobs.
        my $fake_typeglob = ( $fake_package->{$symbol} ||= {} );
        
        # Tie arrays and hashes.
        for my $slot ( 'ARRAY', 'HASH' ) {
            my $reference = *{$typeglob}{$slot};
            if ( defined $reference ) {
                $fake_typeglob->{$slot} = Test::Mimic::Recorder::wrap( $records, $reference, 1, 0 );
            }
        }
    }
    
    # Combine the user selected scalars with the the exported scalars.
    my %all_scalars;
    if ( $package->isa('Exporter') ) {
        no strict 'refs';
        for my $symbol ( @{ $package . '::EXPORT' }, @{ $package . '::EXPORT_OK' } ) {
            if ( substr( $symbol, 0, 1 ) eq '$' ) {
                $all_scalars{ substr( $symbol, 1 ) } = ARBITRARY;
            }
        }
    }
    for my $scalar (@user_selected_scalars) {
        $all_scalars{$scalar} = ARBITRARY;
    }
    
    # Tie all scalars.
    for my $scalar ( keys %all_scalars ) {
        $fake_package->{$scalar}->{'SCALAR'} =
            Test::Mimic::Recorder::wrap( $records, *{$symbol_table->{$scalar}}{'SCALAR'}, 1, 0 );
    }
    
    #Handle inheritance issues regarding both isa and can.
    my ( $full_ISA, $all_subs ) = _get_hierarchy_info($package);
    $extra{$package}{'ISA'} = $full_ISA; 
 
    # Wrap all subroutines. (Not in the sense of the wrap method above, but rather redefine each subroutine
    # to record the operation of the original.)
    for my $sub ( keys %{$all_subs} ) {
        my $original_sub = $package->can($sub);
        my $record_to = ( $fake_package->{$sub}->{'CODE'} ||= {} );
        
        # Define the new subroutine
        my $wrapper_sub = sub {
            
            # Discard calls while recording is suspended, i.e. make the call, but don't record it.
            if ($Test::Mimic::Recorder::SuspendRecording) {
                goto &{$original_sub};
            }
            
            # TODO: Query user settings regarding the volatility of the arguments.
            my $wrapped_args = Test::Mimic::Recorder::wrap( $records, \@_, 1, 1 );
            
            # Set up the recording storage for this call.
            my $arg_key = Test::Mimic::Recorder::stringify($wrapped_args);
            my $context_to_result = ( $record_to->{$arg_key} ||= [] );
            
            # Make actual call, trap exceptions or store return.
            local $Test::Mimic::Recorder::SuspendRecording = 1; # Suspend recording. We don't wan't to record
                                                                # internal calls or state modifications.
            my $context = wantarray();
            my $context_index;
            my $exception;
            my @results;
            my $stored_result;
            my $failed;
            my $eval_error = Devel::EvalError->new();
            $eval_error->ExpectOne(
                eval {
                    if ($context) {
                        $context_index = LIST_CONTEXT;
                        @results = &{$original_sub};
                        $stored_result =
                            [ RETURN, Test::Mimic::Recorder::wrap( $records, \@results, 1, 1) ];
                    }
                    elsif (defined $context) {
                        $context_index = SCALAR_CONTEXT;
                        $results[0] = &{$original_sub};
                        $stored_result =
                            [ RETURN, Test::Mimic::Recorder::wrap( $records, $results[0], 1, 0 ) ];
                    }
                    else {
                        $context_index = VOID_CONTEXT;
                        &{$original_sub};
                        $stored_result = [RETURN];
                    }
                    1;
                }
            );
            $failed = $eval_error->Failed();
            if ( $failed ) {
                $exception = ( $eval_error->AllReasons() )[-1];
                $stored_result = [ EXCEPTION, Test::Mimic::Recorder::wrap( $records, $exception, 1, 0 ) ];
            }
            
            # Maintain records
            push( @operation_sequence, [ $package, CODE_E, $sub, $arg_key, $context_index ] );
            push( @{ $context_to_result->[$context_index] ||= [] }, $stored_result );

            # Propagate original behavior
            if ( $failed ) {
                die $exception;
            }
            elsif ($context) {
                return @results;
            }
            elsif ( defined $context ) {
                return $results[0];
            }
            else {
                return;
            }
        };
        
        # Redefine the original subroutine
        {
            no warnings 'redefine';
            no strict 'refs';
            *{ $package . '::' . $sub } = $wrapper_sub;
        }
    }
}

# Accepts a class name. Returns the class hierarchy flattened into a hash ref and a list of all subroutines
# the class responds too (including inherited subroutines, excluding AUTOLOADED subroutines) also as a hash
# ref. An arbitrary element will exist in the proper hash iff the class isa element or the class can element
# for classes and subroutines respectively. The subroutine names are not fully qualified.
sub _get_hierarchy_info {
    my ($class) = @_;
    
    my %full_ISA = ( $class => ARBITRARY ); # Certainly $class isa $class.
    my %full_subs;
    
    # Find all the subroutines declared in the class.
    my $symbol_table;
    {
        no strict 'refs';
        $symbol_table = \%{ $class . '::' };
    }
    for my $symbol ( keys %{$symbol_table} ) {
        if ( defined *{$symbol_table->{$symbol}}{'CODE'} ) {
            $full_subs{$symbol} = ARBITRARY;
        }
    }
    
    # Get a copy of the actual @ISA array.
    my @true_ISA;
    {
        no strict 'refs';
        @true_ISA = @{ $class . '::ISA' };
    }
    
    # Look through the class hierarchy for all ancestor classes and inherited subroutines.
    for my $parent (@true_ISA) {
        if ( ! exists $full_ISA{$parent} ) {
            my ( $parent_full_ISA, $parent_full_subs ) = _get_hierarchy_info($parent);
            
            # Merge in the parent information.
            @full_ISA{ keys %{$parent_full_ISA} } = values %{$parent_full_ISA};
            @full_subs{ keys %{$parent_full_subs} } = values %{$parent_full_subs};
        }
    }
    
    return ( \%full_ISA, \%full_subs );
}

# Accepts a single argument. Returns true iff the argument is a regular expression created by qr.
sub _is_pattern {
    return Data::Dump::Streamer::regex($_[0]);
}

# Watches if possible (recursively as needed), otherwise returns the single element wrap.
sub _watch {
    my ( $records, $val ) = @_;
    my ( $references, $address_to_index, $alive ) = @{$records};
    
    my $type = reftype($val);
    if ( ! $type ) { # If this is not a reference...
        return [ STABLE, $val ];
    }
    else {
        my $address = refaddr($val);
        my $index;
        if ( defined( $alive->{$address} ) ) {  # If we are watching this reference...
            $index = $address_to_index->{$address};
        }
        else {
            # Note that we are watching the reference.
            $alive->{$address} = $val;
            weaken( $alive->{$address} );   # This reference will be automatically set to undef when $$val is
                                            # garbage collected.
            
            # Create a representation of the reference depending on its type.
            # Watches recursively as necessary via the tie.
            my $reference;
            if ( Test::Mimic::Recorder::_Implementation::_is_pattern($val) ) {
                $reference = [ REG_EXP, $val ];
            }        
            elsif ( $type eq 'SCALAR' || $type eq 'REF' || $type eq 'LVALUE' || $type eq 'VSTRING' ) {
                my $history = [];
                tie( ${$val}, 'Test::Mimic::Recorder::Scalar', $records, $history, ${$val} );
                $reference = [ SCALAR, $history ];                                                          
            }
            elsif ( $type eq 'ARRAY' ) {
                my $history = [];
                tie ( @{$val}, 'Test::Mimic::Recorder::Array', $records, $history, $val );
                $reference = [ ARRAY, $history ];
            }
            elsif ( $type eq 'HASH' ) {
                my $history = [];
                tie ( %{$val}, 'Test::Mimic::Recorder::Hash', $records, $history, $val );
                $reference = [ HASH, $history ];
            }
            else {
                $reference = _handle_simple_types( $val, $type );
            }
            
            # Store the representation of the reference into the references table.
            $index = $address_to_index->{$address} = $#{$references} + 1;
            $references->[$index] = $reference;
        }
        my $class = blessed($val);
        return $class ? [ OBJECT, $class, $index ] : [ VOLATILE, $index ];
    }
    die "We should never reach this point.";    
}

# Performs an expansion wrap on the passed value until the given level then watches every component below.
# Returns a structure analogous to the original except that each component is recursively wrapped. This should
# only be used on static data. If circular references exist above the watch level or into the wrap level the
# behavior is undefined.
#
# For example if _watch was passed an array it would perhaps return [ VOLATILE, 453 ].
# _wrap_then_watch would return [ NESTED, [ ARRAY, [ [ STABLE, 'foo' ], [ STABLE, 'bar' ] ] ] ]
#
# This is useful when the data currently in the array is important, but the array itself has no special
# significance.
#
# TODO: Handle circular references, also save space on DAGs.
# Idea: Scan through structure. Record all references in a big hash. If we see duplicates note them.
# The duplicates will exist as a special structure.
#
# [ CIRCULAR_NESTED, <dupe_table>, [ ARRAY, blah...
# We have one additional type:
# [ DUPE, <index> ]
sub _wrap_then_watch {
    my ( $records, $val, $at_level ) = @_;
    my ( $references, $address_to_index, $alive ) = @{$records};
    
    if ( $at_level == 0 ) { # If we have reached the volatile layer...
        return _watch( $records, $val );
    }
    else {
        $at_level--;
    }
    
    my $type = reftype($val);
    if ( ! $type ) { # If the value is not a reference...
        return [ STABLE, $val ]; 
    }
    else {
        my $wrap;
        if ( Test::Mimic::Recorder::_Implementation::_is_pattern($val) ) {
            $wrap = [ REG_EXP, $val ];
        }        
        elsif ( $type eq 'SCALAR' || $type eq 'REF' || $type eq 'LVALUE' || $type eq 'VSTRING' ) {
            $wrap = [ SCALAR, _wrap_then_watch( $records, ${$val}, $at_level ) ];                                                          
        }
        elsif ( $type eq 'ARRAY' ) {
            my @temp;
            for my $element ( @{$val} ) {
                push( @temp, _wrap_then_watch( $records, $element, $at_level ) );
            }
            $wrap = [ ARRAY, \@temp ];
        }
        elsif ( $type eq 'HASH' ) {
            my %temp;
            for my $key ( keys %{$val} ) {
                $temp{$key} = _wrap_then_watch( $records, $val->{$key}, $at_level );
            }
            $wrap = [ HASH, \%temp];
        }
        else {
            $wrap = _handle_simple_types( $val, $type );
        }
        
        return [ NESTED, $wrap ];
    }
}

# Wraps types that can not be monitored or do not require monitoring.
sub _handle_simple_types {
    my ( $val, $type ) = @_;
    
    my $container;
    if ( $type eq 'CODE' ) {
        $container = [ CODE, $val ];
    }
    elsif ( $type eq 'IO' ) {
        $container = [ IO, $val ];
    }
    elsif ( $type eq 'GLOB' ) {
        $container = [ GLOB, $val ];
    }
    elsif ( $type eq 'FORMAT' ) {
        $container = [ FORMAT, $val ];
    }
    else {
        die "FATAL ERROR: The type of $val is unknown. Unable to watch.";
    }
    
    return $container;
}

# Untested and deprecated. It no longer looks like this will be necessary.
#
# Returns the depth of the shallowest blessed reference or undefined if no blessed reference is found.
# The passed reference must not be to a structure containing any circular links.
sub _contains_blessed {
    my ($val) = @_;
    
    my $type = reftype($val);
    if ( ! $type) {
        return undef;
    }
    else {
        if ( blessed($val) ) {
            return 0;
        }
        else {
            if ( $type eq 'REF' ) {
                my $depth = _contains_blessed(${$val});
                return defined $depth ? $depth + 1 : undef;
            }
            elsif ( $type eq 'HASH' ) {
                my @key_list = keys %{$val};
                my $min = _contains_blessed( $val->{ shift(@key_list) } );
                for my $key (@key_list) {
                    my $depth = _contains_blessed( $val->{$key} );
                    if ( defined $depth && $depth < $min ) {
                        $min = $depth;
                    }
                }
                return defined $min ? $min + 1 : undef;
            }
            elsif ( $type eq 'ARRAY' ) {
                my $min = _contains_blessed( $val->[0] );
                for ( my $i = 1; $i < @{$val}; $i++ ) {
                    my $depth = _contains_blessed( $val->[$i] );
                    if ( defined $depth && $depth < $min ) {
                        $min = $depth;
                    }
                }
                return defined $min ? $min + 1 : undef;
            }
            else { #This doesn't handle globs, but this is irrelevant until wrap handles them fully.
                return undef;
            }
        }
    }
}

# Write recording to disk
END {
    Test::Mimic::Recorder::finish()
        if ( ! $done_writing );
}

1;
__END__

=head1 NAME

Test::Mimic::Recorder - Perl extension for recording the behavior of perl packages. Typically used in
conjunction with Test::Mimic.

=head1 SYNOPSIS

  use Test::Mimic::Recorder qw< -f recording47 -s scalar1,scalar2 Package1 -s scalar_a,scalar_b PackageA >;

=head1 DESCRIPTION

Test::Mimic::Recorder allows a user to monitor the behavior of a set of packages well enough to recreate
that behavior at a later date with reasonable fidelity. Each subroutine, package array and package hash is
monitored. Package scalars will be monitored if specified or if the package inherits from Exporter and they
appear in the @EXPORT or @EXPORT_OK arrays.

For each quadruple of package, subroutine, argument and context a history of results will be stored. These
results can consist of either return values or exceptions. Package variables have their history stored in a
slightly different fashion. Every time a variable is read the resulting value will be stored. Reading a
variable can take on different forms depending on the variable's type. For instance, a scalar can merely be
read, but a hash can have a particular one of its elements read, one of its keys read, the existence of one
of its elements read and so forth. Writes are not recorded.

=head2 SUBROUTINES

=over 4

=item import LIST

Accepts a LIST of packages to record. The file to record to can be specified with -f. Scalars to monitor from
each package can be specified by immediately preceding the package name in the LIST with -s scal_a,scal_b.
The $ sigil should not be included in the scalar name.

Additional options describing the volatility of particular arguments, package variables, etc. are likely to
be incorporated shortly. A hash based import will also be made available for older versions of Perl.

=item finish

Writes all data recorded so far to disk immediately. May be overwritten later. Recorded data is saved
automatically, so this is probably only useful for testing purposes. 

=back

=head3 Protected

Test::Mimic::Recorder should be subclassed and the following methods overridden to modify the manner in which
the recorded data is stored as a string and hence written to a file. The default behavior is to use
Data::Dump::Streamer.

=over 4

=item stringify($value)

Accepts a single scalar. Returns the scalar as a string. Suitable for use as a hash key as well
as being invertible with destringify. Within reason this should be independent of any particular run of the
program.

=item destringify($stringified_value)

The inverse of stringify.

=back

=head3 Private to the Test::Mimic Project

Only Test::Mimic, Test::Mimic::Generator, Test::Mimic::Recorder and Test::Mimic::Verifier should attempt to
call these subroutines.

=over 4

=item wrap($records, $value, $is_volatile, $at_level )

$records is an array reference initially containing [ [], {}, {} ]. The same $records variable should be used
during a given recording session. In a playback session only the first element must be from a recording
session (or rather reconstructed from a recording session). The hash references can again initially be {}.

$value can be either a reference or a simple scalar. wrap will attempt to encapsulate
the value in such a manner that it can be stringified safely. This encapsulated value is returned as an array
reference.

Care is taken to preserve the uniqueness of references (specifically blessed references), so that the
behavior of subroutines dependent on particular references is preserved as well. The obvious use of this is
to allow object methods to be handled properly.

When a mutable reference is wrapped we will begin recording a history of its dereferenced value. This allows
the user to recreate (approximately) the state of the reference over time. This process is recursive and even
complex nested and circular structures should be handled.

However, some action is required on the part of the user. $is_volatile should be set to true if mutable
data is to be monitored. Additionally, $at_level should be set to the highest level where mutable data is to
be expected. For instance, a level of 0 would mean that the passed value itself should be monitored. If an
array reference was passed along with a level of 1 then the elements of the array would be monitored.
Additionally, any circular references need to be handled as mutable data.

=item unwrap

The inverse of wrap.

=back

=head2 EXPORT

None by default.

=head1 SEE ALSO

Test::Mimic
Test::Mimic::Generator
Test::Mimic::Verifier

=head1 AUTHOR

Brendan Roof, E<lt>broof@whitepages.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by WhitePages Inc.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.

=cut
