subroutine kalman(mu, Sigma, H, INFO, R, Sigma_2, data, mu_2, k, n)
implicit none

integer, intent(in)    :: k, n
real*8,  intent(in)    :: Sigma(n,n), H(k,n), mu(n)
real*8,  intent(inout) :: data(k)      ! data, H*mu - data , (H*Sigma*H^T + R)^-1*(H*mu - data)
real*8,  intent(inout) :: R(k, k)      ! R, H*Sigma*H^T + R
integer, intent(out)   :: INFO         ! INFO
real*8,  intent(out)   :: Sigma_2(n,n) ! H^T*(H*Sigma*H^T + R)^-1*H, Sigma, Sigma*(I - H^T*(H*Sigma*H^T + R)^-1*H*Sigma)
real*8,  intent(out)   :: mu_2(n)      ! mu, Sigma*H^T*(H*Sigma*H^T + R)^-1*(H*mu - data) + mu
real*8                 :: H_2(k,n)     ! H * Sigma , H , (H*Sigma*H^T + R)^-1*H
real*8                 :: chol_R(k,k)  ! R, U where (H*Sigma*H^T + R)=U^T*U
real*8                 :: H_data(n)    ! H^T*(H*Sigma*H^T + R)^-1*(H*mu - data)
real*8                 :: N_N_tmp(n,n) ! H^T*(H*Sigma*H^T + R)^-1*H*Sigma

call dsymm('R', 'U', k, n, 1, Sigma, n, H, n, 0, H_2, n)            ! H_2     := 1. * H   * Sigma +  0. * H_2
call dgemm('N', 'T', k, k, n, 1, H_2, n, H, n, 1, R, k)             ! R       := 1. * H_2 * H     +  1. * R
call dgemm('N', 'N', k, 1, n, 1, H, n, mu, 1, -1, data, 1)          ! data    := 1. * H   * mu    + -1. * data
call dcopy(k*n, H, 1, H_2, 1)                                       ! H_2     := H
call dcopy(k*k, R, 1, chol_R, 1)                                    ! chol_R  := R
call dposv('U', k, n, chol_R, k, H_2, n, INFO)                      ! chol_R  := U where R = U^T * U
                                                                    ! H_2     := R^-1 * H_2
call dpotrs('U', k, 1, chol_R, k, data, 1, INFO)                    ! data    := R^-1 * data
call dgemm('T', 'N', n, n, k, 1, H, n, H_2, n, 0, Sigma_2, n)       ! N_N_tmp := 1. * H^T * H_2    + 0. * N_N_tmp
call dgemm('T', 'N', n, 1, k, 1, H, n, data, 1, 0, H_data, 1)       ! H_data  := 1. * H^T * data   + 0. * H_data
call dcopy(n, mu, 1, mu_2, 1)                                       ! mu_2    := mu
call dsymm('L', 'U', n, 1, 1, Sigma, n, H_data, 1, 1, mu_2, 1)      ! mu_2    := 1. * Sigma * H_data  + 1. * mu_2
call dsymm('R', 'U', n, n, 1, Sigma, n, Sigma_2, n, 0, N_N_tmp, n)  ! N_N_tmp := 1. * N_N_tmp * Sigma   + 0. * N_N_tmp
call dcopy(n**2, Sigma, 1, Sigma_2, 1)                              ! Sigma_2 := Sigma
call dsymm('L', 'U', n, n, -1, Sigma, n, N_N_tmp, n, 1, Sigma_2, n) ! Sigma_2 := -1 * Sigma * N_N_tmp + 1. * Sigma_2

RETURN
END
