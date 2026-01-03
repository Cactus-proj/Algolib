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

taylorcoeffs := proc(expr, x, n)
    local s, k:
    s := convert(series(expr, x=0, n),polynom):
    return [seq(coeff(s, x, k), k=0..n-1)]:
end proc:
    
testmajlist := proc(majcoeffs, exprcoeffs)
    local delta, res:
    delta := majcoeffs - exprcoeffs:
    res := type(delta,'list'('nonnegative')):
    if not res then
        printf( "# bounded: %a\n"
                "# by:      %a\n"
                "# delta=   %a\n",
                exprcoeffs, majcoeffs, delta);
    end if;
    res:
end proc:

testmajseries := proc(maj, expr, z, {prec := 10})  # notice the order
    local majcoeffs, exprcoeffs;
    majcoeffs := evalf(taylorcoeffs(maj, z, prec)):
    exprcoeffs := map(abs, evalf(taylorcoeffs(expr, z, prec))):
    testmajlist(majcoeffs, exprcoeffs):
end proc:

testrecbound := proc(maj, rec, u, n, {prec := 30})  # notice the order
    local majcoeffs, exprcoeffs;
    majcoeffs :=  [seq(evalf(eval(maj)), n=0..prec)];
    exprcoeffs := map(abs, gfun:-rectoproc(rec, u(n), 'list')(prec)):
    testmajlist(majcoeffs, exprcoeffs):
end proc:

# y should be a fp number, otherwise this may or may not work
testabsprec := proc(x, y, prec::posint) 
    Digits := prec+10;
    verify(x, y, neighborhood(Float(1,-prec)));
end proc:


#MakeTest := proc()
#
#    printf("# Testing NumGfun v. %a (%a)\n",
#        NumGfun:-version,
#        StringTools:-FormatTime("%Y-%m-%d %T"));
#    kernelopts('assertlevel'=2);
#
#    module()
#    
#        local
#            testgroupnum, testnum, testgroupname, testname, started,
#            testid;
#        export Start, End, NewGroup;
#        option package;
#
#        testgroupnum := 0;
#        testgroupname := "-";
#        testnum := 1;
#        started := false;
#
#        testid := proc()
#            return sprintf("(%a.%a) %s / %a",
#                testgroupnum, testnum, testgroupname, testname);
#        end proc;
#
#        Start := proc(name:=testnum)
#            if started then error "already started" end if;
#            testname := name;
#            printf("########################################"):
#            printf("########################################\n"):
#            printf("# %s\n", testid());
#            testnum := testnum + 1;
#            started := true;
#        end proc:
#
#        End := proc()
#            started := false;
#            printf("okay\n");
#        end proc:
#
#        NewGroup := proc(name::string)
#            testgroupnum := testgroupnum + 1;
#            testgroupname := name;
#            testnum := 1;
#        end proc;
#
#    end module:
#
#end proc:
#
