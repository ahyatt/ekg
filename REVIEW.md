# Code Review Guidelines for EKG

EKG is a general system that uses the Knowledge Graph for everything.  Is something introduced that could be expressed in schema?  Or, instead, just by tagging?

Minimize complexity and be very suspicious of anything new and complex.  Is there a way it could instead fit into the existing functionality of EKG which is already very powerful?

The docs should always be written for a user, and should explain the various options, but does not need to go into detail on how things work.  Make sure nothing unnecessary is part of the docs.  Also, make sure the docs are flowed well, with the linefeeds in reasonable places.  The best way to do things is to use Emacs itself, using `fill-paragraph`.  Make sure the `texi` version is also generated.


