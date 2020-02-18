# Rhyme
Radiation-Hydrodynamics on Patch-Based Adaptive Mesh Refinement

## Task List (Last Updated on: 2019.08.14)

- [ ] **Domain Discretisation**
  - [ ] **Patch-Based AMR**
    - [x] Chombo Data-Structure
    - [ ] Refining based on given criteria
- [ ] **Hydrodynamics**
  - [ ] **Initial Condition**
    - [x] Loading Rhyme Output
  - [ ] **Boundary Condition** (on Base Grid)
    - [x] Outflow
    - [x] Periodic
    - [x] Reflective
  - [ ] **Drawing**
    - [x] Shapes
      - [x] Sphere
      - [x] Cuboid
      - [x] Prism
    - [x] Perturbations
      - [x] Harmonic
      - [x] Symmetric Decaying
  - [ ] **Hydro Solver**
    - [x] MUSCL-Hancock
      - [x] Data Reconstruction
        - [x] Piece-Wise Linear Function
          - [x] Slope limiters
            - [x] van Leer
            - [x] Minmod
            - [x] Superbee
            - [x] van Albada
      - [x] Evolution
        - [x] CFL
        - [x] Half-Step Extrapolation
      - [x] Riemann Problem
        - [x] Riemann Solver
          - [x] Exact
            - [x] Sampling Intercell Fluxes
      - [x] Time Integration
        - [x] Steady Fluxes
  - [ ] **Thermal Conduction**
- [ ] **Radiative Transfer**

## Parameters

The complete list of parameters (`keywords` and *values*).

- **Initial Condition**
  - `ic_types`
    - *simple*: Uniform grid initialised to zero
      - `ic_grid`: Grid dimensionality of the first level (e.g. 128 128 128 )
      - `ic_box_lengths`: Box lengths and their unit (e.g. 1d0 1d0 1d0 kpc )
      - `ic_nlevels`: Maximum number of refinement levels (e.g. 3)
    - *snapshot*: Loading a given snapshot
      - `ic_box_lengths`: Box lengths and their unit (e.g. 1d0 1d0 1d0 kpc )
      - `ic_nlevels`: Maximum number of refinement levels (e.g. 3)
      - `ic_snapshot_type`
      - `ic_snapshot_path`
- **Runtime Options**
  - `max_nboxes`: Maximum number of boxes in each level (e.g. 1 10 100)
- **Internal Units**
  - `density_unit`: (e.g. 'kg / m^3')
  - `length_unit`: (e.g. 'm')
  - `time_unit`: (e.g. 's')
- **Boundary Condition**
  - `left_bc`: (e.g. periodic)
  - `right_bc`: (e.g. outflow)
  - `bottom_bc`: (e.g. reflective)
  - `top_bc`: (e.g. periodic)
  - `back_bc`: (e.g. outflow)
  - `front_bc`: (e.g. reflective)
- **Thermodynamics**
  - `ideal_gas_type`: (e.g. monatomic or diatomic or polyatomic)
- **CFL**
  - `courant_number`: (e.g. 8d-1)
- **Exact Riemann Solver**
  - `vacuum_pressure`: (e.g. 2.2250738585072014E-308) [internal unit]
  - `vacuum_density`: (e.g. 2.2250738585072014E-308) [internal unit]
  - `tolerance`: (e.g. 1d-6)
  - `n_iteration`: (e.g. 100)
- **Slope Limiter**
  - `slope_limiter`: (e.g. van_leer or minmod or van_albada or superbee)
  - `slope_limiter_omega`: $`\omega \in [-1, 1]`$ (e.g. 0d0)
- **MUSCL-Hancock Solver**
  - `solver_type`: (e.g. memory_intensive or cpu_intensive)
- **Drawing**
  - `canvas`
    - *transparent*
    - *uniform rho u v w p*: (e.g. uniform  0.125d0 0.d0 0.d0 0.d0 1d-1)
  - `shape`
    - *cuboid ledge_x ledge_y ledge_z len_x len_y len_z*: (e.g. cuboid 1 1 1  56 128 1)
      - `shape_filling`:
        - *uniform rho u v w p*: (e.g. uniform  1.d0 0.d0 0.d0 0.d0 1.d0)
    - *sphere origin_x origin_y origin_z radius*: (e.g. sphere  3d0 4d0 5d0 2.34d0)
      - `shape_filling`:
        - *uniform rho u v w p*: (e.g. uniform  1.d0 0.d0 0.d0 0.d0 1.d0)
    - *prism v1_x v1_y v1_z v2x_ v2_y v2_z v3_x v3_y v3_z thickness*: (e.g. prism  56 1 1  56 128 1  72 1 1  1)
      - `shape_filling`:
        - *uniform rho u v w p*: (e.g. uniform  1.d0 0.d0 0.d0 0.d0 1.d0)
    - *smoothed_slab_2d axis pixel_start pixel_end sigma_start sigma_end*: (e.g. smoothed_slab_2d  x  56 72  2d-1 4d-1)
      - `shape_filling`:
        - *rho1 u1 v1 w1 p1 rho2 u2 v2 w2 p2*: (e.g. 0.125d0 0.d0 0.d0 0.d0 1d-1 1.d0 0.d0 0.d0 0.d0 1.d0)
  - `perturb`
    - *harmonic coordinate_type axis A lambda rho u v w p*: (e.g. harmonic cartesian x 0.05 32 0.d0 0.d0 0.d0 0.d0 1.d0)
    - *symmetric_decaying coordinate_type axis A position_px sigma rho u v w p*: (e.g. symmetric_decaying cartesian y 1.d0 72 8 0.d0 0.d0 0.d0 0.d0 1.d0)
- **Chombo Output**
  - `prefix`: (e.g. './prefix')
  - `nickname`: (e.g. 'simulation')
- **Sources**
  - `source_coordinate`: (e.g. 12d-1 34d-1 56d-1)
  - `source_opening_angle`: (e.g. 2 pi 1d0 2d0 3d0)
  - `source_spectral_region`:
    - *power_law lambda_start lambda_end total_energy resolution slope*: (e.g. power_law 1d0 2d0 6.78d9 10 -1.7)
    - *linear lambda_start lambda_end total_energy resolution slope*: (e.g. linear 2.d0 3.d0 5.67d8 15 -1)
    - *line guassian lambda total_energy resolution*: (e.g. line gaussian 3.d0 4.56d7d8 10)
    - *line voigt lambda total_energy resolution*: (e.g. line voigt 3.d0 4.56d7d8 10)


Examples are provided for [1D](parameters_1d.conf.example),
[2D](parameters_2d.conf.example) and [3D](parameters_3d.conf.example).
