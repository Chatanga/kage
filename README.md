Kage / かげ
==========

A personal sandbox for playing with Haskell, (post) modern OpenGL (4.3 Core Profile actually), physics simulation and, quite marginally, Japanese ("kage" means "shadow").

Even if I usually take care of committing the project in a working state, this project is hardly a source of authority regarding the aforementioned themes. It could change in a (far) future, but, at this time, I'm afraid it's mostly junk code. That being said, the Haskell OpenGL binding is severely lacking in documentation. It's not a problem for the low-level raw binding (OpenGLRaw), which is essentially a one-to-one translation of the C API, but it is sometime frustrating for the more Haskellish binding (OpenGL) based on it. Having an existing code example which works is always helpful in this case, just to pick the right function names and way of passing some parameters (texture, buffer...). Beyond that, don't look too closely at the code, both from a Haskell and OpenGL perspective.

![Screen capture](doc/current-state.png "Current state")

Inspiration
-----------

Regarding the OpenGL part, I generally get inspiration from the two sites:

-   [MBSoftWorks](http://www.mbsoftworks.sk/index.php?page=tutorials&series=1)
-   [LearnOpenGL](https://learnopengl.com/)

I don't know if they are the best sources (they are simpler to start with than the OpenGL SuperBible at least),
but since some shader parts and resources actually come from these places,
better give to Caesar what is Caesar's.

The new part related to physics simulation is heavily inspired by the *Game Physics Engine Development* book from Ian Millington (whose [Cylone engine](https://github.com/idmillington/cyclone-physics/) is on GitHub).

Technical notes
---------------

Despite the OpenGL package (3.0.1.0) stating it supports version 4.5, it is only the case for the underlying raw binding. The higher level Haskell binding seems to lack some features, at least regarding Shaders (compute, separate binding). In order to further explore the OpenGL API, I've introduced a `Ext` module. It cannibalizes a minimal set of hidden things from the `Graphics.Rendering.OpenGL` package to be able to introduce some changes: `ExtShader`, `ExtProgram`...

Dependencies
------------

Since a good deal of Haskell packages are just wrappers on native libraries, `stack build` will probably ends up complaining about missing libraries on your system. On mine (Debian based), here is the list of things I’ve had to install:

```
$ sudo apt-get install mesa-common-dev libgl1-mesa-dev libxi-dev libxrandr-dev libxcursor-dev libxinerama-dev libglu-dev zlib1g-dev
```

Lastly, my rendering hardware is a AMD RX 470 with the free AMDGPU driver. No doubt it will work on most graphic cards supporting the OpenGL 4.3 Core Profile.

On Linux Mint 18.1:
```
Renderer: Gallium 0.4 on AMD POLARIS10 (DRM 3.3.0 / 4.8.0-41-generic, LLVM 4.0.0)
OpenGL version: 4.5 (Core Profile) Mesa 17.1.2
Shading language version: 4.50
```

Then currently on Ubuntu 18.04:
```
Renderer: AMD Radeon (TM) RX 470 Graphics (POLARIS10 / DRM 3.23.0 / 4.15.0-22-generic, LLVM 6.0.0)
OpenGL version: 4.5 (Core Profile) Mesa 18.0.0-rc5
Shading language version: 4.50
```

