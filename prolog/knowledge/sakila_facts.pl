% actor(ActorID, Name)
actor(1, 'PENELOPE GUINNESS').
actor(2, 'NICK WAHLBERG').
actor(3, 'ED CHASE').
actor(4, 'JENNIFER DAVIS').
actor(5, 'JOHNNY LOLLOBRIGIDA').

% film(FilmID, Title, Description, ReleaseYear, LengthMinutes)
film(1, 'ACADEMY DINOSAUR', 'A Epic Drama...', 2006, 86).
film(2, 'ACE GOLDFINGER', 'A Astounding...', 2006, 48).
film(3, 'ADAPTATION HOLES', 'A Thoughtful Documentary...', 2006, 86).
film(4, 'AFFAIR PREJUDICE', 'A Witty Romantic Tale...', 2006, 117).
film(5, 'AFRICAN EGG', 'An Exotic Adventure...', 2006, 130).

% acted_in(ActorID, FilmID)
acted_in(1, 1).
acted_in(1, 23).
acted_in(2, 1).
acted_in(3, 2).
acted_in(4, 5).

% category(CategoryID, Name)
category(1, 'Action').
category(2, 'Animation').
category(3, 'Comedy').
category(4, 'Drama').
category(5, 'Documentary').

% film_category(FilmID, CategoryID)
film_category(1, 6).
film_category(2, 11).
film_category(3, 5).
film_category(4, 4).
film_category(5, 1).