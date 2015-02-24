# Glass - stochastic

On calcule aléatoirement soit la refraction soit la reflexion

num aléatoire (compare à 0,5) seulement si on est dans un cas de refraction

possiblement passer un tableau de nombre aléatoire à chaque tableau

--> besoin de normaliser la contribution par 1/0.5 (proba) pour prendre en
compte le fait que on regarde pas toutes les contrib

--> spp = 4



---> image bruitée (calcul aléatoire)

(changer le threshold de probabilité)

bla bla * fR / pdf
u < threshold -> reflection
u > threshlod -> refraction
pdf reflection -> 1/ threshold
pdf refraction = 1 - 1 / threshold
pdf reflection = fR
pdfRefraction = 1 - fR
