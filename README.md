# emacs-kinopoisk

API of [kinopoisk](https://en.wikipedia.org/wiki/Kinopoisk "Link to Wikipedia Article about Kinopoisk") (cinema-service) for Emacs Lisp

## Installing

_NOTE:_ `MELPA` haven't `emacs-kinopoisk` yet

If you need to use `emacs-kinopoisk` in other repo, then: 

1. Clone to the root directory of your project

```shell
cd /path/to/root/of/your/project

# Via `https`

git clone https://github.com/semenInRussia/emacs-kinopoisk.git
```

2. Add the followed require-statement to your source:

```elisp
(require 'kinopoisk)
```

If you need to the usage of `helm-kinopoisk`, then:

1. Clone the repo to the folder with other installed projects:

```shell
# I save installed projects in ~/.emacs.d/lisp/ , change to your folder
cd ~/.emacs.d/lisp/

# Via `https`
git clone https://github.com/semenInRussia/emacs-kinopoisk.git
```

2. If you use [use-package](https://github.com/jwiegley/use-package "a
   Link to the Cool Repo"), the use followed code:
   
   ```elisp
(use-package helm-kinopoisk
    ;; Change the next line if you use a other folder for save the
    ;; Elisp source
    :load-path "~/.emacs.d/lisp/emacs-kinopoisk")
   ```

## Usage
### Usage of `kinopoisk`
Main object of these Elisp package is `kinopoisk-film`, for find a
film from id, use the followed code:

```elisp
;; 301 is ID of film `The Matrix' on Kinopooisk
(kinopoisk-film-from-id 301)
```

If you need to get a info from a `kinopoisk-film`, then use
the followed functions:

* `kinopoisk-film-year` (the type is string)
* `kinopoisk-film-countries` (the type is list of string, list of
  countries in russian language)
* `kinopoisk-film-rating` (the type is number, rating from 0 to 100)
* `kinopoisk-film-id` (the type is number)
* `kinopoisk-film-name` (the type is string, the russian name)
* `kinopoisk-film-original-name` (the type is string)
* `kinopoisk-film-web-url` (the type is string)
* `kinopoisk-film-poster-url` (the type is string)
* `kinopoisk-film-length` (the type is number, the amount of minutes
in film)
* `kinopoisk-film-slogan` (the type is string)
* `kinopoisk-film-description` (the type is string)
* `kinopoisk-film-short-description` (the type is string)
* `kinopoisk-film-rating-age-limits` (the type is string, for
  example: "age16")

If you need to open web page of film in web browser, use function
`kinopoisk-film-open-in-web`

### Usage of `kinopoisk`
Now `helm-kinopoisk` have only one interactive function
`helm-kinopoisk-search-films` with followed HELM actions:

- Just Return
- Open Web Page of Kinopoisk
## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2022 Semen Khramtsov.

[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
