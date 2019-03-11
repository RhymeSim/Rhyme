# Iterative Riemann Solver
Iterative Riemann solver (based on Newton-Raphson iteration method)

## Background
  The Riemann problem for the one-dimensional time-dependent Euler equations
  is an Initial Value Problem (IVP) for conservation laws,

  ```math
  \partial_t U + \partial_x F(U) = 0
  ```

  $`U`$ is the vector of conservative variables and $`F(U)`$ is the vector of
  the fluxes. In 1D, one can write the conservative variables as:

  ```math
  U = \begin{bmatrix}
  \rho \\
  \rho u \\
  E
  \end{bmatrix}, F(U) = \begin{bmatrix}
  \rho u \\
  \rho u^2 + p \\
  u (E + p)
  \end{bmatrix}

  ```

  where $`E = \frac 1 2 \rho u^2 + e_{int}`$ and $`e_{int}`$ is the internal
  energy.

  The solution of this Riemann problem directly depends on the $`x/t`$ ratio
  and consists of three types of waves; two nonlinear, shocks or rarefactions,
  and one linearly degenerate, the contact discontinuity.

  Contact discontinuities are surfaces that separate zones of different density
  and temperature at fixed pressure. By definition such a surface is in pressure
  equilibrium, and no gas flows across it. Usually, when the tangential
  component of velocity on one side differs considerably from that of the gas
  on the other side, the surface is called a slip discontinuity. The boundary
  of a supersonic jet and the ambient gas is an example of a slip discontinuity.

  The other two types of nonlinear waves arise from abrupt changes in pressure.
  Shock fronts accompany compression of the medium and rarefactions accompany
  expansion of the medium.

  These waves are separating four constant states where the conservative
  vector $`U`$ acquires from the left to the right the following values,
  $`U_L`$, $`U_{L^*}`$, $`U_{R^*}`$ and $`U_R`$.
  The symbol $`*`$ identify points located in the star region between the
  nonlinear waves.

  The following two equations help us to solve a Riemann problem,

  ```math
  f_L(p_*, U_L) + f_R(p_*, U_R) + \Delta u = 0
  ```

  ```math
  u^* = \frac 1 2 (u_L + u_R) + \frac 1 2 \left[ f_R(p_*, U_R) - f_L(p_*, U_L) \right]
  ```

  The functions $`f_L`$ and $`f_R`$ represent relations across the left and the
  right nonlinear waves and are given by:

  ```math
  f_K \left(p_*, U_K\right) = \begin{cases}
    (p_* - p_K) \sqrt{(\frac{A_K}{p_* + B_K})} & \text{if } p_* \geq p_K \\
      \\
    \frac{2 c_s^K}{\gamma - 1} \left[
      \left( \frac{p_*}{p_K} \right)^{\frac{\gamma - 1}{2 \gamma}}
      - 1 \right] & \text{if } p_* < p_K
  \end{cases}
  ```

  where $`A_K = \frac{2}{(\gamma + 1) \rho_K}`$ and
  $`B_K = \frac{\gamma - 1}{\gamma + 1} p_K`$. $`p_*`$ can be solved by an
  iterative scheme using the first solution equation. Note that $`p_*`$ is
  a function of five variables, $`p_* (\Delta u, \rho_L, p_L, \rho_R, p_R)`$.
  After finding $`p_*`$, $`u*`$ can easily be calculated by the second
  solution equation. The remaining unknowns are found by the means of standard
  gas dynamic relations.

  The following figure shows the star region, shock (S) or rarefaction (R)
  nonlinear waves, contact discontinuity (C) and flux functions in the $`x-t`$
  plane,

  ![x-t-Riemann-Solver](assets/x-t-Riemann-Solver.jpg)

  The unknown region between $`u_L`$ and $`u_R`$ is the star region. Note that
  the middle wave is always a contact discontinuity while the left and right
  nonlinear waves are either shock or rarefaction waves. Therefore, according
  to the type of the nonlinear waves, there can be four possible wave
  configurations,

  ![x-t-Riemann-Solver](assets/four-cases.png)

  Note that both pressure $`p_*`$ and velocity $`u_*`$ between left and right
  waves are constant, while the density takes on the two constant values
  $`\rho_{*L}`$ and $`\rho_{*R}`$.

  So far, using an iterative scheme, we can find $`p_*`$ and $`u_*`$.


### Finding $`p_*`$ iteratively
  Our aim is to minimize or cancel the residual, $`R(p)`$, of the following
  formula using an iterative scheme (here the Newton-Raphson iteration method):

  ```math
  f_L(p, U_L) + f_R(p, U_R) + \Delta u = R(p)
  ```

  Newton-Raphson method requires the value of the function and its derivatives. Derivatives can be calculated as:

  ```math
  f_K^\prime \left(p_*, U_K\right) =
  \begin{cases}
    \sqrt{(\frac{A_K}{p + B_K})} \left( 1 - \frac{p - p_K}{2 (B_k + p)} \right)
      & \text{if } p \geq p_K \\
    &  \\
    \frac{1}{\rho_K c_s^K}
      \left( \frac{p}{p_K} \right)^{\frac{- (\gamma + 1)}{2 \gamma}}
      & \text{if } p < p_K
  \end{cases}
  ```

  The most computationally expensive step is the calculation of the terms
  with fractional exponents. Considering the common terms of the
  function and its derivatives, we can avoid repeating these calculations.

  Finally, the Newton-Raphson iteration can be simply written as,

  ```math
  p_{i+1} = p_i - \frac{R_{(p_i)}}{R^\prime_{(p_i)}}
  ```

### Calculating $`u_*`$ based on a given $`p_*`$
  When we find $`p_*`$, we are ready to calculate the velocity in the star
  region,

  ```math
  u_* = \frac 1 2 \left( u_R - u_L \right) + \frac 1 2 \left(f_R - f_L \right)
  ```
### Sampling the Solution
  By identifying types of the left and the right nonlinear waves, we are able
  to compute $`\rho_{*L}`$ and $`\rho_{*R}`$. In order to specify the type of
  a nonlinear wave, we should compare the star region pressure, $`p_*`$, with
  the left or the right pressure, $`p_L`$ and $`p_R`$.

  For shock waves, we calculate the density behind the shock and the shock
  speed.

  For rarefactions, we calculate the density behind the wave, an equation
  for the head and the tail of the wave and the full solution inside
  the rarefaction region.

  Here we go through different types of nonlinear waves:

#### Left shock wave ( $`p_* > p_L`$ )
  **density**
  ```math
  \rho_{*L} = \rho_L \left[
    \frac{\frac{p_*}{p_L} + \frac{\gamma - 1}{\gamma + 1}}
      {\frac{\gamma - 1}{\gamma + 1}\frac{p_*}{p_L} + 1}
  \right]
  ```

  **shock speed**
  ```math
  S_L = u_L - Q_L / \rho_L, \quad Q_L = \left[ \frac{p_* + B_L}{A_L} \right]^{\frac 1 2}
  ```

  Therefore, $`S_L`$ can be written as,

  ```math
  S_L = u_L - c_s^L \left[
    \frac{\gamma + 1}{2\gamma} \frac{p_*}{p_L} + \frac{\gamma - 1}{2\gamma}
    \right]^{\frac 1 2}
  ```

#### Left Rarefaction wave ( $`p_* \leq p_L`$ )
  **density** follows the isentropic law,
  ```math
  \rho_{*L} = \rho_L \left( \frac{p_*}{p_L} \right)^{\frac{1}{\gamma}}
  ```

  **sound speed behind the rarefaction**
  ```math
  c_s^{*L} = c_s^L \left( \frac{p_*}{p_L} \right)^{\frac{\gamma - 1}{2\gamma}}

  ```

  **head and tail speeds**
  ```math
  S_{HL} = u_L - c_s^L, S_{TL} = u_* - c_s^{*L}
  ```

#### Right shock wave ( $`p_* > p_R`$ )
  **density**
  ```math
  \rho_{*R} = \rho_R \left[
    \frac{\frac{p_*}{p_R} + \frac{\gamma - 1}{\gamma + 1}}
      {\frac{\gamma - 1}{\gamma + 1}\frac{p_*}{p_R} + 1}
  \right]
  ```

  **shock speed**
  ```math
  S_R = u_R + Q_R / \rho_R, \quad Q_R = \left[ \frac{p_* + B_R}{A_R} \right]^{\frac 1 2}
  ```

  Therefore, $`S_R`$ can be written as,

  ```math
  S_R = u_R - c_s^R \left[
    \frac{\gamma + 1}{2\gamma} \frac{p_*}{p_R} + \frac{\gamma - 1}{2\gamma}
    \right]^{\frac 1 2}
  ```

#### Right Rarefaction wave ( $`p_* \leq p_R`$ )
  **density** follows the isentropic law,
  ```math
  \rho_{*R} = \rho_R \left( \frac{p_*}{p_L} \right)^{\frac{1}{\gamma}}
  ```

  **sound speed behind the rarefaction**
  ```math
  c_s^{*R} = c_s^R \left( \frac{p_*}{p_L} \right)^{\frac{\gamma - 1}{2\gamma}}

  ```

  **head and tail speeds**
  ```math
  S_{HR} = u_R - c_s^R, S_{TR} = u_* - c_s^{*R}
  ```

## Test cases
  **Sod's test**
  A left rarefaction, a contact discontinuity and a right shock

  **123 test (Einfeldt et al. 1991)**
  Two strong rarefactions and a trivial stationary contact discontinuity.
  p* is too small and close to vacuum

  **Left half of the blast wave problem from Woodward and Colella**
  A left rarefaction, a contact and a (vary strong) right shock

  **Right half of the blast wave problem from Woodward and Colella**
  A left shock, a contact and a (vary strong) right rarefaction

  **Collision of the shocks emerging from blast waves**
  A left facing shock (travelling very slowly to the right), a right
  travelling contact discontinuity and a right travelling shock wave

  **degeneracy in the solution of a Riemann problem** ( need to be implemented )
  Consider the following dimensionless parameters,

  ```math
  \pi_1 = \frac{\Delta u}{\sqrt{\frac{p_R}{\rho_R}}},
  \pi_2 = \frac{p_L}{p_R},
  \pi_3 = \frac{\rho_L}{\rho_R}
  ```

  (Here we used the right state as the reference).

  Riemann problems with identical $`\pi_1`$, $`\pi_2`$ and $`\pi_3`$,
  must have similar behaviour. Therefore, the ratio $`p_*/p_R`$
  must have the same value for all cases.
