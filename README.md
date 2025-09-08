### OuiWisp

A Lisp Dialect build from the ground up specifically made to help C-dev to develop their 
compositors or GUI framework for the wayland protocol.

## Why?... just Why?

This project is just for educational-porpouse only, it probably never will be ready for 
production because its a messy, complicated and maybe impossible experiment.
So, please, dont critique the nature of this project because i dont even take it a as a valid 
option compared to simply programming C with existing libraries like wlroots, libwayland etc.

Alright, picking up from there and steering toward the “functional approach to graphics” inside OuiWisp’s model.

Functional Graphics in OuiWisp
In Symploke we can treat rendering as a pure transformation: a program reduces to a value 
that describes the frame, and a small “port” in the VM performs the side-effect of presenting that frame.

At a high level:
- GC State: a persistent, immutable value describing scene state (entities, transforms, materials…).
- RenderSpec: a pure value describing what to draw this frame (passes, clear ops, draw calls, state).
- CommandList: a canonicalized, low-level description the runtime submits to the GPU.
- DisplayPort: the VM’s effect boundary that takes a CommandList and does the real I/O.

So conceptually: ProgramState × Inputs → World → RenderSpec → CommandList → [DisplayPort submits]


So the aim of this project is try to prove why functional graphics is viable and possible can be used to 
generate .c and .h files automatically without the programmer needing to write the implementation by hand 
and simply describing **how everything needs to be** and **not how needs to work**.

Obviusly this is too idealistic for only one little guy that doesent even know everything about how tall and 
scary programmer work at their desk at important company. So take this mostly as joke, a "j'accuse" to all that 
people that said to me that "Lisp isnt worth it", nothing more really! I swear!

# A little client-side example (schematic)
(def output-0 (output "HDMI-A-1"))
(def surface-0 (surface :title "OuiWisp" :size (800 600)))

(render
(pass main
(clear 0.1 0.1 0.1 1.0)
(draw surface-0 basic-pipeline
:uniforms ((mvp (mul P (mul V M)))
(tint (1 0.5 0.2 1)))
:target output-0)))

(commit output-0) ; wl_surface_commit + wlr_output_commit, under the hood
