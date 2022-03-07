module type_point

    type,public :: Point
        real :: x,y
    end type Point

    interface operator(.equal.)
        module procedure is_equal_points
    end interface

    contains

    function is_equal_points(p1,p2)
        logical :: is_equal_points
        type(Point),intent(in) :: p1,p2
        if(p1%x == p2%x .and. p1%y == p2%y) then
            is_equal_points = .TRUE.
        else
            is_equal_points = .FALSE.
        end if
    end function

end module type_point


module type_set
    use type_point

    type,public :: Set
        type(Point),allocatable :: points(:)
    end type Set

    interface operator(+)
        module procedure sum_of_sets
    end interface

    contains

    function sum_of_sets(s1,s2)
        type(Set) :: sum_of_sets
        type(Set) :: s_res, s_large, s_small
        type(Set),intent(in) :: s1,s2
        type(Point),allocatable :: points_r(:),points(:)
        integer :: i,j,n,m,k = 1
        logical :: tmp1,tmp2
        tmp2 = .FALSE.

        allocate(points(size(s1%points) + size(s2%points)))

        if(size(s1%points) > size(s2%points)) then
            s_large = Set(s1%points)
            s_small = Set(s2%points)
        else
            s_large = Set(s2%points)
            s_small = Set(s1%points)
        end if

        n = size(s_large%points)
        m = size(s_small%points)

        do i = 1, n, 1
            points(i) = s_large%points(i)
        end do

        do j = 1, m, 1
            do i = 1, n, 1
                tmp1 = points(i) .equal. s_small%points(j)
                if(tmp1 .eqv. .TRUE.) then
                    tmp2 = tmp1
                end if
            end do
            if(tmp2 .eqv. .FALSE.) then
                points(n + k) = s_small%points(j)
                k = k + 1
            end if
            tmp2 = .FALSE.
        end do

        allocate(points_r(n + k - 1))

        do i = 1, size(points_r),1
            points_r(i) = points(i)
        end do

        deallocate(points)

        s_res = Set(points_r)

        deallocate(points_r)

        sum_of_sets = s_res

    end function
end module type_set
