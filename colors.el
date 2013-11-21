(setq my-color-themes (list 'color-theme-aalto-dark 'color-theme-aalto-light 'color-theme-aliceblue
                            'color-theme-analyze-defun 'color-theme-andreas 'color-theme-arjen
                            'color-theme-bharadwaj 'color-theme-bharadwaj-slate 'color-theme-billw
                            'color-theme-black-on-gray 'color-theme-blippblopp 'color-theme-blue-eshell
                            'color-theme-blue-gnus 'color-theme-blue-mood 'color-theme-blue-sea
                            'color-theme-calm-forest 'color-theme-charcoal-black 'color-theme-clarity
                            'color-theme-classic 'color-theme-comidia 'color-theme-compare
                            'color-theme-dark-blue 'color-theme-dark-blue2 'color-theme-dark-erc
                            'color-theme-dark-font-lock 'color-theme-dark-gnus 'color-theme-dark-green
                            'color-theme-dark-info 'color-theme-dark-laptop 'color-theme-deep-blue
                            'color-theme-describe 'color-theme-digital-ofs1 'color-theme-emacs-21
                            'color-theme-emacs-nw 'color-theme-euphoria 'color-theme-example
                            'color-theme-feng-shui 'color-theme-fischmeister 'color-theme-gnome
                            'color-theme-gnome2 'color-theme-goldenrod 'color-theme-gray1
                            'color-theme-gray30 'color-theme-greiner 'color-theme-gtk-ide
                            'color-theme-high-contrast 'color-theme-hober 'color-theme-infodoc
                            'color-theme-initialize 'color-theme-jb-simple 'color-theme-jedit-grey
                            'color-theme-jonadabian 'color-theme-jonadabian-slate 'color-theme-jsc-dark
                            'color-theme-jsc-light 'color-theme-jsc-light2 'color-theme-katester
                            'color-theme-kingsajz 'color-theme-late-night 'color-theme-lawrence
                            'color-theme-ld-dark 'color-theme-lethe 'color-theme-marine
                            'color-theme-marquardt 'color-theme-matrix 'color-theme-midnight
                            'color-theme-mistyday 'color-theme-montz 'color-theme-oswald
                            'color-theme-parus 'color-theme-pierson 'color-theme-pok-wob
                            'color-theme-pok-wog 'color-theme-ramangalahy 'color-theme-raspopovic
                            'color-theme-renegade 'color-theme-resolve 'color-theme-retro-green
                            'color-theme-retro-orange 'color-theme-robin-hood 'color-theme-rotor
                            'color-theme-ryerson 'color-theme-salmon-font-lock 'color-theme-scintilla
                            'color-theme-select 'color-theme-shaman 'color-theme-simple-1
                            'color-theme-sitaramv-nt 'color-theme-sitaramv-solaris 'color-theme-snow
                            'color-theme-snowish 'color-theme-standard 'color-theme-subtle-blue
                            'color-theme-subtle-hacker 'color-theme-taming-mr-arneson 'color-theme-taylor
                            'color-theme-tty-dark 'color-theme-vim-colors 'color-theme-whateveryouwant
                            'color-theme-wheat 'color-theme-word-perfect 'color-theme-xemacs
                            'color-theme-xp))

 (defun my-theme-set-default () ; Set the first row
      (interactive)
      (setq theme-current my-color-themes)
      (funcall (car theme-current)))
     
    (defun my-describe-theme () ; Show the current theme
      (interactive)
      (message "%s" (car theme-current)))
     
   ; Set the next theme (fixed by Chris Webber - tanks)
    (defun my-theme-cycle ()		
      (interactive)
      (setq theme-current (cdr theme-current))
      (if (null theme-current)
      (setq theme-current my-color-themes))
      (funcall (car theme-current))
      (message "%S" (car theme-current)))
    
    (setq theme-current my-color-themes)
    (setq color-theme-is-global nil) ; Initialization
    (my-theme-set-default)
