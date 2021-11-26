module random

    use cdf_normal_mod
    use cdf_chisq_mod

    implicit none

    private

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0d0)

    integer, parameter :: wp = dp

    type, public :: trand

    contains

        private

        procedure, pass, private :: stduniform
        procedure, pass, public :: uniform

        procedure, pass, public :: d2Drv_gen
        procedure, pass, public :: d2Drv_matrix

        procedure, pass, private :: exp1
        procedure, pass, public :: mean
        generic, public :: expectation => exp1, mean

        procedure, pass, private :: var1, var2
        generic, public :: variance => var1, var2

        procedure, pass, private :: cov1, cov2
        generic, public :: covariance => cov1, cov2

        procedure, pass, private :: pcc1, pcc2
        generic, public :: correlation => pcc1, pcc2

        procedure, pass, public :: conf_int_exp
        procedure, pass, public :: conf_int_var

    end type trand

contains
    ! -----------------------------------------------------------------------------------------------------------------
    ! Discrete two-dimensional random variable generator
    ! -----------------------------------------------------------------------------------------------------------------
    function d2Drv_gen(this, prob, vals_1st, vals_2nd) result (rv)
        ! -------------------------------------------------------------------------------------------------------------
        ! Parameters:
        !
        !   Input, real(wp) prob(:,:), probability matrix ||prob(i,j)|| (i = 1,m; j = 1,n).
        !   Where prob(i,j) - the probability of the joint appearance of the i-th and j-th values
        !                     of the first and second components respectively.
        !
        !   Input, real(wp) vals_1st(:), vector of possible values of the 1st component.
        !
        !   Input, real(wp) vals_2nd(:), vector of possible values of the 2nd component.
        !
        ! Return:
        !
        !   real(wp) rv(2), discrete 2D random variable.
        ! -------------------------------------------------------------------------------------------------------------

        implicit none

        class(trand):: this

        real(wp), intent(in) :: vals_1st(:), vals_2nd(:), prob(:,:)

        real(wp) :: row_sums(size(prob, dim=1)), cumsum_1st(size(prob, dim=1)), cumsum_2nd(size(prob, dim=2))
        real(wp) :: rv(2)
        integer :: i, j, idx_1st, idx_2nd

        row_sums = sum(prob, dim=2)
        cumsum_1st = [(sum(row_sums(1:i)), i = 1,size(row_sums))]
        idx_1st = minloc(cumsum_1st, dim=1, mask=(cumsum_1st >= this%uniform(0._wp, 1._wp)))
        cumsum_2nd = [(sum(prob(idx_1st, 1:j)), j = 1,size(prob, dim=2))] / row_sums(idx_1st)
        idx_2nd = minloc(cumsum_2nd, dim=1, mask=(cumsum_2nd >= this%uniform(0._wp, 1._wp)))

        rv(1) = vals_1st(idx_1st); rv(2) = vals_2nd(idx_2nd)

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Empirical probability matrix
    ! -----------------------------------------------------------------------------------------------------------------
    subroutine d2Drv_matrix(this, prob, vals_1st, vals_2nd, amount, empirical, samples, counts)
        ! -------------------------------------------------------------------------------------------------------------
        ! Parameters:
        !
        !   Input, real(wp) prob(:,:), probability matrix ||prob(i,j)|| (i = 1,m; j = 1,n).
        !   Where prob(i,j) - the probability of the joint appearance of the i-th and j-th values
        !                     of the first and second components respectively.
        !
        !   Input, real(wp) vals_1st(:), vector of possible values of the 1st component.
        !
        !   Input, real(wp) vals_2nd(:), vector of possible values of the 2nd component.
        !
        !   Input, integer amount, number of samples.
        !
        !   Output, real(wp) empirical(:,:), empirical probability matrix.
        !
        !   Output, real(wp) samples(2,amount), 1st and 2nd random variables values.
        !
        !   Output, integer counts(:,:), 1st and 2nd random variables values stats.
        ! -------------------------------------------------------------------------------------------------------------

        implicit none

        class(trand):: this

        real(wp), intent(in) :: vals_1st(:), vals_2nd(:), prob(:,:)
        integer, intent(in) :: amount

        real(wp), intent(out) :: empirical(:,:), samples(2,amount)
        integer, intent(out) :: counts(:,:)

        real(wp) :: rv(2)
        integer :: i, idx_1st, idx_2nd

        counts = 0
        do i = 1, amount
            rv = this%d2Drv_gen(prob, vals_1st, vals_2nd)

            samples(:,i) = rv
            idx_1st = findloc(vals_1st, dim=1, value=rv(1))
            idx_2nd = findloc(vals_2nd, dim=1, value=rv(2))

            counts(idx_1st, idx_2nd) = counts(idx_1st, idx_2nd) + 1
        end do

        empirical = real(counts,wp) / amount

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Standard uniform function
    ! -----------------------------------------------------------------------------------------------------------------
    subroutine stduniform(this, u)

        implicit none

        class(trand):: this
        real(wp), intent(out) :: u
        real(wp) :: r

        call random_number(r)
        u = 1 - r

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Uniform distribution
    ! -----------------------------------------------------------------------------------------------------------------
    real(wp) function uniform(this, a, b)

        implicit none

        class(trand):: this
        real(wp), intent(in) :: a, b
        real(wp) :: temp, u

        call this%stduniform(u)
        if(a <= b) then
            uniform = (b - a) * u + a
        else
            uniform = (a - b) * u + b
        end if

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Population expectation
    ! -----------------------------------------------------------------------------------------------------------------
    real(wp) function exp1(this, prob, vals)

        implicit none

        class(trand):: this
        real(wp), intent(in) :: prob(:), vals(:)

        exp1 = sum(prob*vals)

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Standart mean (sample expectation)
    ! -----------------------------------------------------------------------------------------------------------------
    real(wp) function mean(this, array) ! exp2

        implicit none

        class(trand):: this
        real(wp), intent(in) :: array(:)

        mean = sum(array)/size(array)

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Population variance (discrete random variable)
    ! -----------------------------------------------------------------------------------------------------------------
    real(wp) function var1(this, prob, vals)

        implicit none

        class(trand):: this
        real(wp), intent(in) :: prob(:), vals(:)

        var1 = this%exp1(prob,vals**2)-this%exp1(prob,vals)**2

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Sample variance
    ! -----------------------------------------------------------------------------------------------------------------
    real(wp) function var2(this, array)

        implicit none

        class(trand):: this
        real(wp), intent(in) :: array(:)

        var2 = sum((array-this%mean(array))**2)/(size(array)-1)

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Confidence interval for sample expectation
    ! -----------------------------------------------------------------------------------------------------------------
    function conf_int_exp(this, array, conf_level)

        implicit none

        class(trand):: this
        real(wp), intent(in) :: array(:), conf_level
        real(wp), dimension(2) :: conf_int_exp
        real(wp) :: mean, var, conf

        mean = this%mean(array)
        var = this%var2(array)
        conf = inv_normal((1+conf_level)/2._dp)*sqrt(var/size(array))

        conf_int_exp(1) = mean-conf
        conf_int_exp(2) = mean+conf

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Confidence interval for sample variance
    ! -----------------------------------------------------------------------------------------------------------------
    function conf_int_var(this, array, conf_level)

        implicit none

        class(trand):: this
        real(wp), intent(in) :: array(:), conf_level
        real(wp), dimension(2) :: conf_int_var
        real(wp) :: mean

        mean = this%mean(array)

        conf_int_var(1) = sum((array-mean)**2)/inv_chisq(ccum=(1-conf_level)/2.,df=real(size(array),dp))
        conf_int_var(2) = sum((array-mean)**2)/inv_chisq(ccum=(1+conf_level)/2.,df=real(size(array),dp))

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Population covariance (discrete random variable)
    ! -----------------------------------------------------------------------------------------------------------------
    real(wp) function cov1(this, prob, vals_1st, vals_2nd)

        implicit none

        class(trand):: this
        real(wp), intent(in) :: prob(:,:)
        real(wp), intent(in) :: vals_1st(:), vals_2nd(:)
        real(wp) :: exp_1st, exp_2nd
        integer :: i, j, n, m

        m = size(vals_1st)
        n = size(vals_2nd)
        exp_1st = this%exp1(sum(prob, dim=2), vals_1st)
        exp_2nd = this%exp1(sum(prob, dim=1), vals_2nd)
        cov1 = sum([((prob(i,j)*(vals_1st(i)-exp_1st)*(vals_2nd(j)-exp_2nd), j = 1,n), i = 1,m)])

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Sample covariance
    ! -----------------------------------------------------------------------------------------------------------------
    real(wp) function cov2(this, samples)

        implicit none

        class(trand):: this
        real(wp), intent(in) :: samples(:,:)
        real(wp) :: exp_1st, exp_2nd

        exp_1st = this%mean(samples(1,:))
        exp_2nd = this%mean(samples(2,:))
        cov2 = dot_product(samples(1,:)-exp_1st,samples(2,:)-exp_2nd)/(size(samples(1,:))-1)

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Population Pearson correlation coefficient
    ! -----------------------------------------------------------------------------------------------------------------
    real(wp) function pcc1(this, prob, vals_1st, vals_2nd)

        implicit none

        class(trand):: this
        real(wp), intent(in) :: prob(:,:)
        real(wp), intent(in) :: vals_1st(:), vals_2nd(:)
        real(wp) :: std_dev_1st, std_dev_2nd

        std_dev_1st = sqrt(this%var1(sum(prob, dim=2), vals_1st))
        std_dev_2nd = sqrt(this%var1(sum(prob, dim=1), vals_2nd))
        pcc1 = this%cov1(prob, vals_1st, vals_2nd)/std_dev_1st/std_dev_2nd

    end
    ! -----------------------------------------------------------------------------------------------------------------
    ! Sample Pearson correlation coefficient
    ! -----------------------------------------------------------------------------------------------------------------
    real(wp) function pcc2(this, samples)

        implicit none

        class(trand):: this
        real(wp), intent(in) :: samples(:,:)
        real(wp) :: std_dev_1st, std_dev_2nd

        std_dev_1st = sqrt(this%var2(samples(1,:)))
        std_dev_2nd = sqrt(this%var2(samples(2,:)))
        pcc2 = this%cov2(samples)/std_dev_1st/std_dev_2nd

    end
end module random