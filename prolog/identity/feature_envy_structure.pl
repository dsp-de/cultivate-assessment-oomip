:- module(cqjv_feature_envy_structure, [
    % Anchor
    executable/1, 
    % Roles
    access/2, accessed_field/2, own_class/2, accessed_own_field/2, 
    accessed_foreign_field/2, accessed_foreign_class/2,
    % Relations
    executable_contains_access/3, accesses/3, accessed_field_is_in_class/3,
    is_in/3, extends/3,
    % Metrics
    access_to_own_data/2, access_to_foreign_data/2, foreign_data_providers/2,
    locality_of_attribute_access/2,
    % Smell
    feature_envy/4
]).

/** <module> Feature Envy Structure

A method has  the code smell "feature  envy", if it "seems more  interested in a
class other than the one it actually is in[, where the] most common focus of the
envy is the data."  [Fowler 1999, p. 80]

Smell/Metrics: This  module provides a  executable definition of the  code smell
"feature   envy"    (feature_envy/4)   in   methods   and    other   executables
(executable/1). The specification follows the suggestions from R.  Marinescu and
M.  Lanza  in [Lanza, Marinescu  2006, pp.  84-87].  Their  "detection strategy"
does consider data as the focus of  envy and therefore detects the specific form
of feature  envy sometimes  called "data envy".   It is based  on the  number of
accessed fields  from other classes (access_to_foreign_data/2,  ATFD), the ratio
of    accessed     own    fields    compared    to     all    accessed    fields
(locality_of_attribute_access/2, LAA) and the number of foreign classes of which
a    field    is    accessed    by   the    executable    under    consideration
(foreign_data_providers/2, FDP).

Structure:  The definition  of the  metrics including  the complementary  metric
counting the accessed  own fields (access_to_own_data/2, ATOD) are  based on the
feature envy structure defined as follows: An access in the sense of this module
accesses (accesses/3) a field, if it either accesses the field directly or calls
a   method   that   mainly   accesses   a  field   to   access   it   indirectly
(method_mainly_accesses_field/2, subsuming  trivial_getter_method/2).  Depending
on the class that contains the accessed field (accessed_field_is_in_class/3) the
field   is   classified  as   own   or   foreign  field   (accessed_own_field/2,
accessed_foreign_field/2).   Besides  the  class that  contains  the  executable
itself the  superclasses of  this class  or an  direct outer  class are  as well
considered to be own classes (own_class/2).  Any class that contains an accessed
foreign field is considered as foreign class (accessed_foreign_class/2).

--

Example: Method =PolygonFigure.setPointAt(p, i)= from JHotDraw 5.1:

    public class PolygonFigure extends AttributeFigure {

        protected Polygon fPoly = new Polygon();
        
        public  void setPointAt(Point p, int i) {
            willChange();
            fPoly.xpoints[i] = p.x;
            fPoly.ypoints[i] = p.y;
            changed();
        }
    }

[[img/feature_envy_structure_jhotdraw_polygonfigure_setpoint.png]]

  | Metric | Value | Reason                                         | Limits |
  | ATFD   | 4     | =fPoly.xpoints=, =p.x=, =fPoly.ypoints=, =p.y= | > 2    |
  | ATOD   | 1     | =fPoly= (accessed twice, but just one field)   |        |
  | LAA    | 0.2   | = ATOD/(ATOD+ATFD) = 1 / (1 + 4)               | < 0.33 |
  | FDP    | 2     | =Point=, =Polygon=                             | =< 5   |

--

Adaptability:  The  structure  can  be   adapted  by  augmenting  the  following
predicates:

    * method_mainly_accesses_field/2 --  The method  mainly accesses a  field so
      that a call to the method expresses as much envy as a direct field access.
      Trivial accessor  methods (trivial_accessor_method/2), i.e.   methods that
      do nothing but accessing the field, are a prototypical example and already
      subsumed by  this predicate.  Augmenting  this predicate may  increase the
      number of detected smell occurrences, if  the field is a foreign field and
      decrease  if the  field is  an  own field.  In  the former  case ATFD  and
      possibly FDP increases and LAA decreases,  in the latter case ATOD and LAA
      increase.

    * almost_commonly_owned_field/1 --  Fields that  could be  considered almost
      common property  so that no executable  has a reason to  get envious about
      them.  Constants (constant/1) are already considered to be almost commonly
      owned fields,  but the predicate  might as  well get augmented  from other
      modules.  Any access to fields identified by this predicate are completely
      ignored in this structure.
      
    * executable_has_free_access_to_class/2  -- Some  classes  are  meant to  be
      accessed freely by certain executables.  The prototypical example here are
      visit  methods in  the concrete  visitors  in the  visitor pattern.  These
      methods are meant to work on  the attributes of the visited element. While
      this module does not  define any free access the predicate  is meant to be
      augmented from other modules. Any field  in a potential foreign class that
      is matched by this  predicate is ignored as foreign field  and thus by the
      metrics ATFD and LAA. It does not contribute to ATOD either.

--
More  information  about code  smells  in  general  and  about feature  envy  in
particular may be found here:

    * http://martinfowler.com/bliki/CodeSmell.html
    * http://wiki.c2.com/?FeatureEnvySmell
    * http://wiki.c2.com/?DataEnvy
    * [Fowler 1999, p. 80]
    * [Lanza,  Marinescu  2006, pp. 84-87]

--
Implementation notes:

Order:  The predicates  in this  file are  ordered by  their dependency.   Later
predicates depend  on those defined earlier  either in this file  or in imported
modules.
    
Terminology: The  metric definitions  use the terminology  ("data", "attribute",
"foreign") used  in the  literature.  The  only difference is  that we  allow to
apply  the predicates  on any  executable not  only on  methods.  The  structure
definitions  use instead  Java  terminology ("field").   The metaphorical  terms
("envy", "own", "foreign") may have other meaning in other contexts.

Performance: Prolog may deliver for some  of the predicates the same result more
than once.  If there are three accesses to an own field, Prolog will return this
field three times  for the predicate accessed_own_field/2, once for  each of the
possible   derivations.    The   metrics    are   consequently   defined   using
aggregate_all/4  to  "deduplicate  redundant  solutions"  based  on  the  second
argument, i.e.  they  count each occurrence of one value  only once.  Duplicates
could  be avoided  upfront by  enumerating all  potentially accessed  fields and
classes and  then checking for the  existence of an access  from the executable.
While the  check itself is fast,  the number of checks  is inappropriately high.
The number of potentially accessed classes  and fields is high, while the number
of  actually accessed  classes and  fields is  expected to  be rather  moderate.
Avoiding the duplicates upfront would  thus reduce performance.  Non declarative
means to avoid duplicates would affect the clarity of the code.

Evolution: The given variant  of feature envy focuses on the  use of data. There
is some  play in  the implementation  given by the  augmentation points  and the
limits used in the smell definition. Nevertheless, for a variant of feature envy
that  focuses  on  behavior, or  only  on  write  access,  or only  on  combined
read-write-accesses a new implementation should be considered.

@author Daniel Speicher
@license EPL

*/

:- use_module(cultivate_core('cqc'), []).

:- use_module(cultivate_core('utils/math'), [quotient_or_default/4]).

:- use_module(cultivate_java('elements/fields'),          [field_is_in_type/2]).
:- use_module(cultivate_java('abstractions/executables'), [
    executable/1, executable_accesses_field/3, executable_calls_method/3, 
    executable_is_in_type/2]).
:- use_module(cultivate_java('analysis/inheritance'),     [
    extends_directly/2, extends_recursively/2]).
:- use_module(cultivate_java('analysis/nesting'),         [outer_class/2]).
:- use_module(cultivate_java('concepts/field_concepts'),  [constant/1]).
:- use_module(cultivate_java('concepts/method_concepts'), [
    trivial_accessor_method/2]).    

:- use_module(cultivate_assessment('java/java_concepts'), [array_length/1]).

:- use_module('oomip_thresholds', [min_few/1, max_few/1]).


/**
 * method_mainly_accesses_field(Method, Field)
 *
 * The method mainly accesses a field so  that a call to the method expresses as
 * much envy as a direct field  access.  Trivial accessor methods, i.e.  methods
 * that  do nothing  but accessing  the field,  are a  prototypical example  and
 * already subsumed by  this predicate.  Augmenting this  predicate may increase
 * as well as decrease the number of detected smell occurrences.
 */
:- multifile method_mainly_accesses_field/2.

method_mainly_accesses_field(TrivialAccessor, Field) :-
    trivial_accessor_method(TrivialAccessor, Field).


/**
 * almost_commonly_owned_field(Field)
 *
 * The field  is meant  to be  used by any  class, so  that no  executable could
 * become  envious  because  of  this field.   Constants  are  the  prototypical
 * example,  as they  are meant  to be  shared. The  predicate already  subsumes
 * constants.
 */
:- multifile almost_commonly_owned_field/1.

almost_commonly_owned_field(Constant) :- 
    constant(Constant).

almost_commonly_owned_field(ArrayLength) :- 
    array_length(ArrayLength).


/**
 * executable_has_free_access_to_class(Executable, Class)
 *
 * Some executables  are meant to  have free access  to the attributes  of other
 * classes. The prototypical  example here are the visit methods  in the visitor
 * pattern. They  are meant to  work on the  attributes of the  visited element.
 * Augmenting this predicate  is meant to decrease the number  of detected smell
 * occurrences.
 */
:- multifile executable_has_free_access_to_class/2.


cqc:anchor(executable).
%
% reexported from executables
%


cqc:relation(accesses).
/**
 * accesses(E, Access, Field)
 *
 * The access accesses the field.
 */
accesses(E, Access, Field) :-
    executable_accesses_field(E, Access, Field),
    not(almost_commonly_owned_field(Field)).

accesses(E, Call, Field) :-
    executable_calls_method(E, Call, Method),
    method_mainly_accesses_field(Method, Field),
    not(almost_commonly_owned_field(Field)).


cqc:relation(executable_contains_access).
/**
 * executable_contains_access(E, E, Access)
 * 
 * The executable contains an access.
 */ 
executable_contains_access(E, E, Access) :-
    accesses(E, Access, _).


cqc:role(access).
/**
 * access(E, Access)
 *
 * An access in the executable.
 */
access(E, Access) :-
    accesses(E, Access, _).


cqc:role(accessed_field).
/**
 * accessed_field(E, Field)
 * 
 * An accessed field.
 */
accessed_field(E, Field) :- 
    accesses(E, _, Field).


cqc:relation(accessed_field_is_in_class).
/**
 * accessed_field_is_in_class(E, Field, Class)
 *
 * The accessed field is defined in the class.
 */
accessed_field_is_in_class(E, Field, Class) :-
    accesses(E, _, Field),
    field_is_in_type(Field, Class).


cqc:relation(is_in).
/** 
 * is_in(E, Class, OuterClass)
 *
 * Class  is in  the OuterClass.   Especially for  anonymous inner  classes, the
 * access to fields of the outer class is as natural as the access to attributes
 * of the own class in the strict  sense. Nevertheless, we just go out one level
 * and do not consider superclasses of the outer class.
 */
is_in(E, Class, OuterClass) :-
    executable_is_in_type(E, Class),
    outer_class(Class, OuterClass).


cqc:role(own_class).
/**
 * own_class(E, Class) 
 *
 * The own  class of the  executable, a superclass of  this class, or  the outer
 * class if the class is an inner class.
 */
own_class(E, Class) :-
    executable_is_in_type(E, Class). 

own_class(E, SuperClass) :-
    executable_is_in_type(E, Class),
    extends_recursively(Class, SuperClass). 

own_class(E, OuterClass) :-
    is_in(E, _, OuterClass).


cqc:relation(extends).
/** 
 * extends(E, Class, Superclass)
 *
 * Extends relation restricted to own classes of the executable. 
 */
extends(E, Class, Superclass) :-
    own_class(E, Class),
    extends_directly(Class, Superclass).

    
cqc:role(accessed_own_field).
/**
 * accessed_own_field(E, Field)
 *
 * An accessed field a class that is considered an own class of the executable.
 */
accessed_own_field(E, Field) :-
    accessed_field_is_in_class(E, Field, Class),
    own_class(E, Class).


cqc:role(accessed_foreign_field).
/**
 * accessed_foreign_field(E, Field)
 *
 * An accessed field that  is in a class that is not considered  an own class of
 * the executable.
 */
accessed_foreign_field(E, Field) :-
    accessed_field_is_in_class(E, Field, Class),
    not(own_class(E, Class)),
    not(executable_has_free_access_to_class(E, Class)).


cqc:role(accessed_foreign_class).
/**
 * accessed_foreign_class(E, Class)
 * 
 * A class having a field accessed by  the executable, that is not considered an
 * own class.
 */
accessed_foreign_class(E, Class) :-
    accessed_foreign_field(E, Field),
    accessed_field_is_in_class(E, Field, Class).


cqc:metric(executable, access_to_own_data, integer(0, infinity)).
/**
 * access_to_own_data(Executable, ATOD)
 *
 * The number of accessed fields belonging to  a class that is considered an own
 * class of the executable.
 */ 
access_to_own_data(Executable, ATOD) :-
    executable(Executable),
    aggregate_all(count, Field, accessed_own_field(Executable, Field), ATOD).


cqc:metric(executable, access_to_foreign_data, integer(0, infinity)).
/**
 * access_to_foreign_data(Executable, ATFD)
 *
 * The number of accessed fields not belonging  to a class that is considered an
 * own class of the executable.
 */
access_to_foreign_data(Executable, ATFD) :-
    executable(Executable),
    aggregate_all(count, Field, accessed_foreign_field(Executable, Field), ATFD).


cqc:metric(executable, locality_of_attribute_access, float(0.0, 1.0)).
/**
 * locality_of_attribute_access(Executable, LAA)
 *
 * The number  of accessed own  fields divided by  the total number  of accessed
 * fields. When there are no fields accessed at all, we consider the locality to
 * be perfect, i.e. to have the value 1.0.
 */
locality_of_attribute_access(Executable, LAA) :-
    access_to_own_data(Executable, ATOD),
    access_to_foreign_data(Executable, ATFD),
    ATD is ATFD + ATOD,
    quotient_or_default(ATOD, ATD, 1.0, LAA).


cqc:metric(executable, foreign_data_providers, integer(0, infinity)).
/**
 * foreign_data_providers(Executable, FDP)
 *
 * The number of classes containing fields  accessed by the executable, that are
 * not considered own classes of the executable.
 */
foreign_data_providers(Executable, FDP) :-
    executable(Executable),
    aggregate_all(count, Class, accessed_foreign_class(Executable, Class), FDP).


cqc:smell(executable, feature_envy).
/**
 * feature_envy(Executable, SomeFeatures, LowLocality, FewProviders)
 *
 * A method  has feature envy,  when it uses  more foreign attributes  from only
 * some classes than  own attributes. More precisely: An  Executable has feature
 * envy,  if  it accesses  more  than  SomeFeatures  from foreign  classes,  the
 * locality of attribute access is below LowLocality and the attributes are from
 * not  more than  FewProviders.   For the  actual values  of  these limits  see
 * cqc:limits/3.
 */
feature_envy(Executable, SomeFeatures, LowLocality, FewProviders) :-
    access_to_foreign_data(Executable, ATFD),      ATFD >  SomeFeatures, 
    locality_of_attribute_access(Executable, LAA), LAA  <  LowLocality, 
    foreign_data_providers(Executable, FDP),       FDP  =< FewProviders.

cqc:limits(executable, feature_envy, [SomeFeatures, LowLocality, FewProviders]) :-
    min_few(SomeFeatures), LowLocality is 1/3, max_few(FewProviders).