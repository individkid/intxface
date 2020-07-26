#include "plane.h"
struct VertexOutput {
   float4 position [[position]];
   float4 color;
   float3 normal;
   float2 coord;
};
// MAIN
vertex VertexOutput vertex_main (
   const device Plane *plane [[buffer(0)]],
   const device Point *point [[buffer(1)]],
   uint ident [[vertex_id]],
   const device float3 basis[3])
{
   VertexOutput v_out;
   return v_out;
}

