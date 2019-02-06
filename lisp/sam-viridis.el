;;; sam-viridis.el --- viridis palettes access from ivy  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: convenience, colors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(require 'ivy)
(require 'counsel)
(require 'sam-utils)

(defvar sam-viridis--palette-names
  '("magma" "inferno" "plasma" "viridis" "cividis")
  "List of palette names in the viridis package.")

(sam--hash sam-viridis--palettes
  "List of palettes in the viridis package"
  "magma"
  ("#000004" "#07071D" "#160F3B" "#29115A" "#400F73"
   "#56147D" "#6B1D81" "#802582" "#952C80" "#AB337C"
   "#C03A76" "#D6456C" "#E85362" "#F4685C" "#FA815F"
   "#FD9A6A" "#FEB37B" "#FECC8F" "#FDE4A6" "#FCFDBF")
  "inferno"
  ("#000004" "#08051E" "#190C3E" "#300A5B" "#460B6A"
   "#5C126E" "#711A6E" "#87216B" "#9C2964" "#B1325A"
   "#C43C4E" "#D64B40" "#E55C30" "#F17020" "#F8870E"
   "#FCA007" "#FBB91F" "#F7D340" "#F1ED6F" "#FCFFA4")
  "plasma"
  ("#0D0887" "#2D0594" "#44039E" "#5901A5" "#6F00A8"
   "#8305A7" "#9512A1" "#A72197" "#B6308B" "#C5407E"
   "#D14E72" "#DD5E66" "#E76E5B" "#EF7F4F" "#F79044"
   "#FBA238" "#FEB72D" "#FDCB26" "#F7E225" "#F0F921")
  "viridis"
  ("#440154" "#481568" "#482677" "#453781" "#3F4788"
   "#39558C" "#32648E" "#2D718E" "#287D8E" "#238A8D"
   "#1F968B" "#20A386" "#29AF7F" "#3CBC75" "#56C667"
   "#74D055" "#94D840" "#B8DE29" "#DCE318" "#FDE725")
  "cividis"
  ("#00204D" "#002A64" "#00336F" "#1F3C6D" "#35466B"
   "#444F6B" "#53596C" "#5F636E" "#6B6C71" "#777776"
   "#838079" "#908B79" "#9D9677" "#ABA074" "#B9AC70"
   "#C7B76B" "#D7C463" "#E5D05A" "#F5DD4D" "#FFEA46"))

(defun sam-viridis--format-palettes ()
  (let* ((pals sam-viridis--palette-names)
       (get-col
        (lambda (col)
          (mapconcat
           (lambda (c)
             (propertize (make-string 3 ?\s) 'face
                         (list :background c :foreground c)))
           (gethash col sam-viridis--palettes) "")))
       (fmt (format "%%-%ds %%s" (apply #'max 0 (mapcar #'length pals))))
       (cols (mapcar (lambda (x) (format fmt x (funcall get-col x))) pals)))
  (replace-regexp-in-string
   " " ""
   (completing-read "" cols))))


(defun sam-viridis--choose-palette (&optional palette)
  (let ((pal (gethash
              (or palette
                  (sam-viridis--format-palettes))
              sam-viridis--palettes)))
    (cl-loop
     for str in pal
     collect (propertize "   " 'hex (downcase str)))))

;;;###autoload
(defun sam-viridis (colors)
  (interactive
   (list (sam-viridis--choose-palette)))
  (let*
      ((blank (make-string 10 ?\s))
       (fmt (format "%%-%ds %%s %%s" 1))
       (action (lambda (str) (insert (upcase (get-text-property 0 'hex str)))))
       (ivy-format-function
        (counsel-colors--formatter
         (lambda (color)
           (let ((hex (get-text-property 0 'hex color)))
             (format fmt color
                     (propertize hex 'face (list :foreground hex))
                     (propertize blank 'face (list :background hex))))))))
    (ivy-read
     "Choose color:" colors
     :action (lambda (x) (funcall action x)))))


(provide 'sam-viridis)
;;; sam-viridis.el ends here
