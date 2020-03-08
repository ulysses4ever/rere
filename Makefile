doctest :  
	doctest -DRERE_INTERSECTION src

slow-doctest :  
	doctest -DRERE_INTERSECTION -DRERE_SLOW_DOCTEST src
