# Copyright (C) 1991--2013 by INRIA.
#
# This file is part of Algolib.
#
# Algolib is free software: you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Algolib is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Algolib.  If not, see
# <http://www.gnu.org/licenses/>.

with(TestTools):

with(gfun):


# The first version of my code failed on singular recurrences.  This example
# comes from Bruno's email <4D76EDA9-FD61-4FA9-9C68-949CE3207284@inria.fr>.

rec := {(-25/18*n-5/3+25/18*n^2+25/18*n^3+5/18*n^4)*u(n)+(115/3-29/18
    *n^4-67/6*n^3-113/6*n^2+239/18*n)*u(n+1)+(-1265/6+61/18*n^4+181/6
    *n^3+623/9*n^2-767/18*n)*u(n+2)+(408-55/18*n^4-601/18*n^3-865/9
    *n^2+506/9*n)*u(n+3)+(-250+n^4+13*n^3+45*n^2-25*n)*u(n+4),
    u(0) = 1, u(1) = 5/8, u(2) = 5/12, u(3) = 169/576, u(4) = 3899/18000,
    u(5) = 1199/7200, u(6) = 163571/1234800};

pp := rectoproc(rec, u(n));

Try(1.1,
    [seq(pp(n), n=0..8)],
    [1, 5/8, 5/12, 169/576, 3899/18000, 1199/7200, 163571/1234800,
    17483/161280, 38441/423360]);

Try(1.2,
    pp(100), # above threshold (in current implementation)
    774819396611888457089420685380037874706165988817058414333428390683904974845893277/
    297736269186400098150393913270251559970640154023661300949699353241023092165757440000
    );

# Large singular index.

rec := {(n-100)*u(n+1) = u(n), u(0)=1, u(101)=2};

pp := rectoproc(rec, u(n));

Try(2.1,
    [pp(1), pp(2), pp(10), pp(100), pp(101), pp(102), pp(106), pp(107), pp(108), pp(130)],
    [-1/100, 1/9900, 1/62815650955529472000,
    1/93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000,
    2, 2, 1/60, 1/360, 1/2520, 1/4420880996869850977271808000000]);

