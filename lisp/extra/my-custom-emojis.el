;;; my-custom-emojis.el --- List of custom emojis   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;;; Commentary:
;;
;; This file is a list whith my custom emojis list, used on `emoji'.
;;
;;
;; ███████╗███╗   ███╗ ██████╗      ██╗██╗███████╗       ██╗
;; ██╔════╝████╗ ████║██╔═══██╗     ██║██║██╔════╝    ██╗╚██╗
;; █████╗  ██╔████╔██║██║   ██║     ██║██║███████╗    ╚═╝ ██║
;; ██╔══╝  ██║╚██╔╝██║██║   ██║██   ██║██║╚════██║    ██╗ ██║
;; ███████╗██║ ╚═╝ ██║╚██████╔╝╚█████╔╝██║███████║    ╚═╝██╔╝
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝  ╚════╝ ╚═╝╚══════╝       ╚═╝
;;
;;; Code:

(defun concat-emacs-directory (str)
  "Concat `user-emacs-directory' with STR."
  (concat user-emacs-directory str))

(defvar my-custom-emojis
  `(
    ;;; PEPE COLECTION
    (":pepe-angry-ping:"   .    (("name" . "Angry ping Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-angry-ping.png"))
                                 ("style" . "github")))

    (":pepe-batman:"       .    (("name" . "Batman Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-batman.png"))
                                 ("style" . "github")))

    (":pepe-clown:"        .    (("name" . "Clown Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-clown.png"))
                                 ("style" . "github")))

    (":pepe-cringe:"       .    (("name" . "Cringe Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-cringe.png"))
                                 ("style" . "github")))

    (":pepe-fat-sad"       .    (("name" . "Fat Sad Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-fat-sad.png"))
                                 ("style" . "github")))

    (":pepe-finger:"       .    (("name" . "Finger Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-finger.png"))
                                 ("style" . "github")))

    (":pepe-happy:"        .    (("name" . "Happy Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-happy.png"))
                                 ("style" . "github")))

    (":pepe-knife:"        .    (("name" . "Knife Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-knife.png"))
                                 ("style" . "github")))

    (":pepe-laughing:"     .    (("name" . "Laughing Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-laughing.png"))
                                 ("style" . "github")))

    (":pepe-ok:"           .    (("name" . "OK Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-ok.png"))
                                 ("style" . "github")))

    (":pepe-look:"         .    (("name" . "Looking Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-look.png"))
                                 ("style" . "github")))

    (":pepe-punch:"        .    (("name" . "Punch Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-punch.png"))
                                 ("style" . "github")))

    (":pepe-rage:"         .    (("name" . "Rage Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-rage.png"))
                                 ("style" . "github")))

    (":pepe-sad:"          .    (("name" . "Sad Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-sad.png"))
                                 ("style" . "github")))

    (":pepe-smug:"         .    (("name" . "Smug Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-smug.png"))
                                 ("style" . "github")))

    (":pepe-sunglasses:"   .    (("name" . "Sunglasses Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-sunglasses.png"))
                                 ("style" . "github")))

    (":pepe-sweat"         .    (("name" . "Sweat Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-sweat.png"))
                                 ("style" . "github")))

    (":pepe-super-rage:"   .    (("name" . "Rage Super Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-super-rage.png"))
                                 ("style" . "github")))

    (":pepe-think"         .    (("name" . "Weary Think")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-think.png"))
                                 ("style" . "github")))

    (":pepe-weary:"        .    (("name" . "Weary Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-weary.png"))
                                 ("style" . "github")))

    (":pepe-yugi:"         .    (("name" . "Yu Gi Oh Pepe")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/pepe-emojis/pepe-yugi.png"))
                                 ("style" . "github")))


    ;;; CHAD COLECTION
    (":yeschad:"           .    (("name" . "Yes Chad")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/chad-emojis/yes-chad.png"))
                                 ("style" . "github")))

    (":gigachad:"          .    (("name" . "Gigachad")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/chad-emojis/gigachad.png"))
                                 ("style" . "github")))

    (":gigachad-alt:"      .    (("name" . "Gigachad alt")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/chad-emojis/gigachad-2.png"))
                                 ("style" . "github")))

    (":chad-squidward:"    .    (("name" . "Squidward Chad")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/chad-emojis/gigachad-2.png"))
                                 ("style" . "github")))

    ;;; Programing
    (":archlinux:"         .    (("name" . "ArchLinux")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/program-emojis/archlinux.png"))
                                 ("style" . "github")))

    (":clojure:"           .    (("name" . "Clojure")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/program-emojis/clojure.png"))
                                 ("style" . "github")))

    (":emacs:"             .    (("name" . "Emacs")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/program-emojis/emacs.png"))
                                 ("style" . "github")))

    (":golang:"            .    (("name" . "Golang")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/program-emojis/golang.png"))
                                 ("style" . "github")))

    (":java:"              .    (("name" . "Java")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/program-emojis/java.png"))
                                 ("style" . "github")))

    (":lambda:"            .    (("name" . "Lambda")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/program-emojis/lambda.png"))
                                 ("style" . "github")))

    (":linux:"             .    (("name" . "Linux")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/program-emojis/linux.png"))
                                 ("style" . "github")))

    (":org-mode"           .    (("name" . "Org Mode")
                                 ("image" . ,(concat-emacs-directory "emojis/luiznux/program-emojis/org-mode.png"))
                                 ("style" . "github")))


    ;;; Magic the Gathering
    (":mtg-0:"                   .    (("name" . "0 number mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-0.png"))
                                       ("style" . "github")))

    (":mtg-1:"                   .    (("name" . "1 number mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-1.png"))
                                       ("style" . "github")))

    (":mtg-2:"                   .    (("name" . "2 number mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-2.png"))
                                       ("style" . "github")))

    (":mtg-3:"                   .    (("name" . "3 number mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-3.png"))
                                       ("style" . "github")))

    (":mtg-4:"                   .    (("name" . "4 number mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-4.png"))
                                       ("style" . "github")))

    (":mtg-5:"                   .    (("name" . "5 number mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-5.png"))
                                       ("style" . "github")))

    (":mtg-6:"                   .    (("name" . "6 number mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-6.png"))
                                       ("style" . "github")))

    (":mtg-7:"                   .    (("name" . "7 number mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-7.png"))
                                       ("style" . "github")))

    (":mtg-8:"                   .    (("name" . "8 number mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-8.png"))
                                       ("style" . "github")))

    (":mtg-9:"                   .    (("name" . "9 number mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-9.png"))
                                       ("style" . "github")))

    (":mtg-10:"                  .    (("name" . "10 number mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-10.png"))
                                       ("style" . "github")))

    (":mtg-x:"                   .    (("name" . "X mtg symbol")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-x.png"))
                                       ("style" . "github")))

    (":mtg-tap:"                 .    (("name" . "Tap mtg symbol")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-tap.png"))
                                       ("style" . "github")))

    (":mtg-black:"               .    (("name" . "Black mana mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-black.png"))
                                       ("style" . "github")))

    (":mtg-white:"               .    (("name" . "White mana mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-white.png"))
                                       ("style" . "github")))

    (":mtg-red:"                 .    (("name" . "Red mana mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-red.png"))
                                       ("style" . "github")))

    (":mtg-blue:"                .    (("name" . "Blue mana mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-blue.png"))
                                       ("style" . "github")))

    (":mtg-green:"               .    (("name" . "Green mana mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-green.png"))
                                       ("style" . "github")))

    (":mtg-black-phyrexiana:"    .    (("name" . "Black phyrexiana mana mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-bp.png"))
                                       ("style" . "github")))

    (":mtg-white-phyrexiana:"    .    (("name" . "White phyrexiana mana mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-wp.png"))
                                       ("style" . "github")))

    (":mtg-red-phyrexiana:"      .    (("name" . "Red phyrexiana mana mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-rp.png"))
                                       ("style" . "github")))

    (":mtg-blue-phyrexiana:"     .    (("name" . "Blue phyrexiana mana mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-bp.png"))
                                       ("style" . "github")))

    (":mtg-green-phyrexiana:"    .    (("name" . "Green phyrexiana mana mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-gp.png"))
                                       ("style" . "github")))

    (":mtg-now:"                 .    (("name" . "X mtg symbol")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-s.png"))
                                       ("style" . "github")))

    (":mtg-colorless:"           .    (("name" . "Colorless mana mtg")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-colorless.png"))
                                       ("style" . "github")))

    (":mtg-logo:"                .    (("name" . "Mtg Logo")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/mtg-emojis/mtg-logo.png"))
                                       ("style" . "github")))


    ;;; Others
    (":pain-naruto:"             .    (("name" . "Bad Animation Pain naruto")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/others-emojis/pain-naruto.png"))
                                       ("style" . "github")))

    (":jango:"                   .    (("name" . "Jango Fett")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/others-emojis/jango.png"))
                                       ("style" . "github")))

    (":gas-cylinder:"            .    (("name" . "Gas Cylunder")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/others-emojis/gas-cylinder.png"))
                                       ("style" . "github")))

    (":my-phone:"                .    (("name" . "Mobile Phone custom")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/others-emojis/mobile-phone-custom.png"))
                                       ("style" . "github")))

    (":paypal:"                  .    (("name" . "Paypal")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/others-emojis/paypal.png"))
                                       ("style" . "github")))

    (":stonks:"                  .    (("name" . "Stonks")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/others-emojis/stonks.png"))
                                       ("style" . "github")))

    (":yugioh-card:"             .    (("name" . "Yu Gi Oh Card Back")
                                       ("image" . ,(concat-emacs-directory "emojis/luiznux/others-emojis/yugioh-card-back.png"))
                                       ("style" . "github")))))


(provide 'my-custom-emojis)
;;; my-custom-emojis.el ends here
