// This is a Cabal file template that is used to generate the final cabal file
// for this project using the C preprocessor. Use the `autogen` script from
// the project root directory to do the generation.
//
// This file has the same terms for use and distribution as other files
// in this software. See file LICENSE for details.

// This file defines common stanzas that are used to define build dependencies
// for the project build components. It allows sharing the same version bounds
// accross all components of the same project without having to declare them
// separately (and keep the declarations synchronized)  for aeah component.

common dep-base
    build-depends: base ^>= { 4.18, 4.19 }
common dep-bytestring
    build-depends: bytestring >=0.11.4 && <0.13
common dep-effectful
    build-depends: effectful-core ^>={ 2.2.2, 2.3 }
common dep-hashable
    build-depends: hashable ^>=1.4.1
common dep-pqueue
    build-depends: pqueue ^>=1.5.0
common dep-text
    build-depends: text >=2.0 && <2.2
common dep-unordered-containers
    build-depends: unordered-containers ^>= 0.2.19.1
common dep-random
    build-depends: random ^>= 1.2.1.1
common dep-vector
    build-depends: vector ^>=0.13
