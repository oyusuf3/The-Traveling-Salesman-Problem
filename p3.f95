!Program #3, cssc4864, Omar Yusuf
PROGRAM Traveling
IMPLICIT NONE


! Declaring variables

INTEGER :: number, numOfCities, I, J, first, last, minDistance,status, permutation
CHARACTER(len = 1000) :: filename
CHARACTER(len = 10000), DIMENSION (:), ALLOCATABLE :: listOfNames
INTEGER, DIMENSION (:), ALLOCATABLE :: directions, bestDirections
INTEGER, DIMENSION (:,:), ALLOCATABLE :: distancesInBetween




!Open the file and checking the files

WRITE (*, '(1x,A)', ADVANCE="NO") "Enter File Name:"
READ *, filename
OPEN(UNIT=12, FILE=filename, STATUS="OLD", ACTION="READ",IOSTAT=status)
IF(status /= 0) THEN
   PRINT *
   PRINT *, "File did not open correctly!"
   stop
END IF

! get the number of cities given from the file
READ (UNIT = 12, FMT = 30) numOfCities


!Allocate memory for all needed arrays

allocate(listOfNames(numOfCities))
allocate(distancesInBetween(numOfCities,numOfCities))
allocate(directions(numOfCities))
allocate(bestDirections(numOfCities))


!Fill in arrays from data file

DO i = 1, numOfCities
   READ (12,'(A)') listOfNames (I)
   directions(i) = i
   DO j = 1, numOfCities
       READ(12, FMT = 30) distancesInBetween(i,j)
   END DO
END DO

! Use recursion to find minimal distance

! Initializing values
minDistance = 10000
bestDirections = 0

PRINT *


CALL permute(2,numOfCities)


!Print out the results which is the best directions with least distance

DO i = 1, numOfCities
   IF (i == numOfCities) THEN
       PRINT '(1X,A,A,A,A,I8,A)', trim(listOfNames(bestDirections(i))), " to ",trim(listOfNames(1)), " -- ",&
       distancesInBetween(bestDirections(i), 1), " miles"
   ELSE
       PRINT '(1X,A,A,A,A,I8,A)', trim(listOfNames(bestDirections(i))), " to ",trim(listOfNames(bestDirections(I+1))), " -- ",&
       distancesInBetween(bestDirections(i),bestDirections(i + 1)), " miles"
   END IF
END DO

PRINT '(1X,A,I8,A)', "Best distance is: ", minDistance, " miles"

! deallocate the values
close (12)
deallocate(listOfNames)
deallocate(distancesInBetween)
deallocate(directions)
deallocate(bestDirections)


30 FORMAT (I6)
200 FORMAT (A)

CONTAINS


!permutation for the names and distance of cities
RECURSIVE SUBROUTINE permute(first, last)
INTEGER :: first, last, i, j, currentMin
IF (first == last) THEN
   currentMin = distancesInBetween(1, directions(2))
   DO i = 2, (last - 1)
       currentMin = currentMin + distancesInBetween(directions(i), directions(i + 1))
   END DO
   currentMin = currentMin + distancesInBetween(directions(last), 1)
   IF (currentMin < minDistance) THEN
       minDistance = currentMin
       DO i=1, numOfCities
           bestDirections(i) = directions(i)
       END DO
   END IF

   
   
 
   
  
   ! Permutation counter
   permutation = permutation + 1
ELSE
   DO i = first, last
       CALL swap(first, i)
       CALL permute(first + 1, last)
       CALL swap(first, i)
   END DO
END IF
END SUBROUTINE permute

! This subroutine swap the values as permutation is occuring

SUBROUTINE swap (first, i)
INTEGER :: temp, first, i

temp = directions(first)
directions(first) = directions(i)
directions(i) = temp
END SUBROUTINE swap


END PROGRAM Traveling