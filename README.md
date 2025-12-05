# Chisel Image Processing Accelerator

A hardware accelerator implemented in Chisel that performs image erosion on 20x20 binary images. The accelerator uses morphological erosion operations where a white pixel remains white only if all its 4-neighbors (up, down, left, right) are also white.

## Authors
- Siamul Omar (s235242)
- Zohaib Asghar (s235250)

**Performance**: Completes in 965 clock cycles

## How to Run

Navigate to the project directory and run:

```bash
# Run all tests
sbt test

# Run specific test
sbt "testOnly SystemTopTester"
sbt "testOnly HelloTester"
```
