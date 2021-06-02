submodule(rhyme_initial_condition) rhyme_ic_load_rhyme_smod
contains
   module subroutine rhyme_initial_condition_load_rhyme(ic, samr, logger)
      implicit none

      type(initial_condition_t), intent(in) :: ic
      type(samr_t), intent(inout) :: samr
      type(logger_t), intent(inout) :: logger

      type(chombo_t) :: ch
      integer :: l, b, v, ofs
      integer :: nboxes, lboxes
      integer :: bdims(NDIM), ub(NDIM), blen
      character(len=16) :: level_name
      integer, allocatable :: boxes(:, :)
      real(kind=8), allocatable :: data(:)

      call rhyme_hdf5_util_open(ch%file, ic%snapshot_path)

      do l = 0, samr%nlevels - 1
         write (level_name, '(A7,I0)') "/level_", l

         allocate (samr%levels(l)%boxes(samr%levels(l)%max_nboxes))

         nboxes = rhyme_hdf5_util_get_table_size(ch%file, trim(level_name)//'/boxes')
         allocate (boxes(6, nboxes))

         call rhyme_hdf5_util_read_table(ch%file, trim(level_name), 'boxes', chid%boxes_headers, boxes)

         if (nboxes > samr%levels(l)%max_nboxes) then
            call logger%err('Number of boxes is less than maximum available', &
                            nboxes, '>', [samr%levels(l)%max_nboxes])
            return
         end if

         lboxes = sum([(product(boxes(4:4 + NDIM - 1, b) + 1), b=1, nboxes)])

         ! Reading data dataset
         allocate (data(NCMP*lboxes))
         call rhyme_hdf5_util_read_1d_dataset(ch%file, trim(level_name)//'/data:datatype=0', data)

#if NDIM == 1
#define RANGE_J
#define RANGE_K
#elif NDIM == 2
#define RANGE_J ,1:ub(2)
#define RANGE_K
#elif NDIM == 3
#define RANGE_J ,1:ub(2)
#define RANGE_K ,1:ub(3)
#endif

         ofs = 0
         do b = 1, nboxes
            samr%levels(l)%boxes(b)%level = l
            samr%levels(l)%boxes(b)%number = b

            bdims = boxes(4:4 + NDIM - 1, b) - boxes(1:NDIM, b) + 1
            blen = product(bdims)
            ub = bdims

            call samr%init_box(l, b, bdims, boxes(1:NDIM, b) + 1, boxes(4:4 + NDIM - 1, b) + 1)

            do v = 1, NCMP
               samr%levels(l)%boxes(b)%cells(1:ub(1) RANGE_J RANGE_K, v) = &
                  real( &
                  reshape(data(ofs + (v - 1)*blen + 1:ofs + v*blen), bdims), &
                  kind=8)
            end do

            ofs = ofs + NCMP*blen
         end do

         deallocate (data)
         deallocate (boxes)
      end do

#if NDIM == 1
#define ALL_CELLS 1:bdims(1)
#elif NDIM == 2
#define ALL_CELLS 1:bdims(1), 1:bdims(2)
#elif NDIM == 3
#define ALL_CELLS 1:bdims(1), 1:bdims(2), 1:bdims(3)
#endif

      bdims = samr%levels(0)%boxes(1)%dims

      call logger%log('rho', '(min, max)', '=', [ &
                      minval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%rho)), &
                      maxval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%rho)) &
                      ])

      call logger%log('rho_u', '(min, max)', '=', [ &
                      minval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%rho_u)), &
                      maxval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%rho_u)) &
                      ])

#if NDIM > 1
      call logger%log('rho_v', '(min, max)', '=', [ &
                      minval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%rho_v)), &
                      maxval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%rho_v)) &
                      ])
#endif

#if NDIM > 2
      call logger%log('rho_w', '(min, max)', '=', [ &
                      minval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%rho_w)), &
                      maxval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%rho_w)) &
                      ])
#endif

      call logger%log('e_tot', '(min, max)', '=', [ &
                      minval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%e_tot)), &
                      maxval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%e_tot)) &
                      ])

      call logger%log('T', '(min, max)', '=', [ &
                      minval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%temp)), &
                      maxval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%temp)) &
                      ])

      call logger%log('fHI', '(min, max)', '=', [ &
                      minval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%ntr_frac_0)), &
                      maxval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%ntr_frac_0)) &
                      ])

#if NSPE > 1
      call logger%log('fHeI', '(min, max)', '=', [ &
                      minval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%ntr_frac_1)), &
                      maxval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%ntr_frac_1)) &
                      ])
#endif

#if NSPE > 2
      call logger%log('fHeII', '(min, max)', '=', [ &
                      minval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%ntr_frac_2)), &
                      maxval(samr%levels(0)%boxes(1)%cells(ALL_CELLS, cid%ntr_frac_2)) &
                      ])
#endif

      call rhyme_hdf5_util_close(ch%file)
   end subroutine rhyme_initial_condition_load_rhyme
end submodule rhyme_ic_load_rhyme_smod
