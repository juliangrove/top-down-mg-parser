## Top-down minimalist parsing: an implementation of Kobele, Gerth, and Hale 2013

Minimalist Grammars (MGs) are a mildly context-sensitive grammar formalism for natural language. This is an implementation of the top-down parser for MGs described in [Kobele, Gerth, and Hale 2013](https://link.springer.com/chapter/10.1007/978-3-642-39998-5_3). It hasn't been endorsed by any of the authors :) Given a lexicon, string, and feature sequence to parse as, the parser prints out the first parse it finds as a list of pairs of expressions from the lexicon and their Gorn addresses in the derived tree.

Having run Main.hs, you will be asked for a text file containing your lexicon, a string, and a feature sequence to parse the string with. Given the example lexicon.txt, you can do, e.g.,

```
   Specify the file containing your lexicon:

     lexicon.txt

   String to parse:

     ashley knows the fluffy dog likes matt

   Feature sequence:

     c
```

If you have [Nix](https://nixos.org/) installed, you can test the parser out in a nix shell by entering the cloned directory and running:

```
    nix-shell --pure shell.nix --run "cabal repl"
	main
```
