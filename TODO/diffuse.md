Hi Greg,

Your first mission, if you accept it (and I'm sure you'll accept it) is to
implement direct lighting.

What does that mean ?

For now, you only need to focus on ``Diffuse`` surfaces.

Suppose you have a point "P" you want to lit.

After intersection, P is :

``
P = ray.origin + t * ray.direction
``

For any surface, the equation of the light is : 

``
F(wi, wo) * dot (N, wo) * LightColor * (1 / (d ^ 2))
``

With :

- wi : the incident vector (i.e: direction to camera). Normalized.
- wo the outgoing vector (i.e: direction to light). Normalized.
- N the normal to the surface (Normalized).
- LightColor : the color of the light
- d the distance between the shading point and the light source
- F depends on the surface. For a Diffuse surface, it does not depends on wi and wo, then :

  ``
  F(wi, wo) = DiffuseColor / pi
  ``

I highly recommend to start with a light source at:

``
lightPos = Vector 50 70 81.6
``

and a lightColor of :

``
lightColor = Vector 5000 5000 5000
``

The position is arbitrary (but inside the box), the color is arbitrary, but
must be enough to compensate for the squared distance term.

Warnings
--------

The value of the dot. You must ensure it is positive (else you subtract
light and the result will sucks). However, a simple clamp (or absolute value)
is not correct, you must ensure that wi and wo are on the same side of the
surface.

The normal is defined by the center of the circle and the point of shading. Be
careful, with the big spheres, you are seeing the inside surface, hence the
normal will not point in the same direction as wi / wo.

Notes
-----

I'll give you a complete course on surface later, but the rule is that F(wi,
wo) * dot(N, wo) must ensure:

``
   Integrale _ Hemisphere F(wi, wo) dot(N, wo) dwo < 1
``

This is the "conservation of energy" law.

A diffuse surface does scatter the same amount of light in every direction.
Hence F = a constant. However, why is there a "1/pi" factor is this constant ?
You may find it using your knowledge of integration.

Note : observe that

`` / Integrale _ Directions F(wi, wo) dot(N, wo) dwo =
   / Integrale _ (Phi = [0, pi/2]) / Integrale _ (Theta = [0, 2pi])  F(wi, wo) cos(phi) sin(phi) dphi dtheta
``

Note the change of variable ``dwo = sin(phi) dphi dtheta``. Amazing no?

Enjoy!

Part II : Shadows
-----------------

Ok, you shaded the surface at point P lit by Light source at position lightPos.

Check if there is an object between P and lightPos to see if your surface is lit or is in the shadow.

