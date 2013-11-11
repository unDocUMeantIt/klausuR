# Copyright 2009-2013 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package klausuR.
#
# klausuR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# klausuR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with klausuR.  If not, see <http://www.gnu.org/licenses/>.


## temporarily turned off most of the roxygen comments
## class docs will remain static until roxygen2 supports "@slot"

# Class klausuR.test
#
# This class is currently an empty placeholder. In future releases, it is planned to contain the actual test items in a list
# like format.
#
# @title S4 class klausuR.test
# @slot items Empty dummy.
# @name klausuR.test,-class
# @aliases klausuR.test-class klausuR.test,-class
#' @import methods
# @keywords classes
# @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @exportClass klausuR.test
#' @noRd
# @rdname klausuR.test-class

setClass("klausuR.test",
	representation=representation(
		items="list"
  ),
  prototype(
		items=list()
  )
)

# setValidity("klausuR.titems", function(object){
# 	obj.test	<- object@test
# })
