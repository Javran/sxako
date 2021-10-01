# A standard Chess implementation

This is a toy project to implement standard Chess for fun.

The project begins with just the concept of [bitboard](https://en.wikipedia.org/wiki/Bitboard#Standard),
and gradually we'll explore known designs, learn and adapt them into this library.

Project status: implementation is complete, tests are pending.

Potential future plans:

- Support UCI protocol therefore communication with Chess engines.
- Support PGN files.
- Solve well-known endgames with "textbook techniques" (in contrast to brute-forcing a shortest mating / drawing line)

## Third-party data

### Chess Font

Font source: [Chess Mérida](http://www.enpassant.dk/chess/fonteng.htm),
converted to SVG Font with fontforge.

### Test data

Some test data are randomly picked from [lichess.org open database](https://database.lichess.org/)

### PGN example

`example.pgn` from [Wikipedia]https://en.wikipedia.org/wiki/Portable_Game_Notation#Example().
