

multiplicationMatrix = matrix ( c (
  0, 0, 1,
  0, 1, 0,
  1, 0, 0
), nrow=3,
byrow=T);


transposeMatrix = function(mat)
{
  t(mat);
}
# Function to rotate 90 degrees
rotateMatrix90 = function(mat)
{
  transposedMatrix = transposeMatrix(mat)
  rotatedMatrix = transposedMatrix%*%multiplicationMatrix
  rotatedMatrix;
}
# Function to rotate 180 degrees
rotateMatrix180 = function(mat)
{
  transposedMatrix = transposeMatrix(mat)
  rotatedMatrix = transposedMatrix%*%multiplicationMatrix
  transposedMatrix = transposeMatrix(rotatedMatrix)
  rotatedMatrix = transposedMatrix%*%multiplicationMatrix
  rotatedMatrix;
}
# Function to rotate 270 degrees
rotateMatrix270 = function(mat)
{
  transposedMatrix = transposeMatrix(mat)
  rotatedMatrix = transposedMatrix%*%multiplicationMatrix
  transposedMatrix = transposeMatrix(rotatedMatrix)
  rotatedMatrix = transposedMatrix%*%multiplicationMatrix
  transposedMatrix = transposeMatrix(rotatedMatrix)
  rotatedMatrix = transposedMatrix%*%multiplicationMatrix
  rotatedMatrix;
}