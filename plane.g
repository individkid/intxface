#include "plane.h"
struct VertexOutput {
   float4 position [[position]];
   half3 normal;
   half4 color;
   half2 coord;
};
float3 expand(float3 plane, uint versor, float3 basis[3][3])
{
   return float3(0.0); // TODO
}
float3 intersect(float3 point[3][3])
{
   return float3(0.0); // TODO
}
float3 normal(float3 point[3])
{
   return float3(0.0); // TODO
}
float3 average3(float3 point[3])
{
   return float3(0.0); // TODO
}
// MAIN
vertex VertexOutput vertex_main (
   const device Plane *plane [[buffer(0)]],
   const device Point *point [[buffer(1)]],
   uint ident [[vertex_id]],
   const device State *state)
{
   VertexOutput v_out;
   float3 basis[3][3];
   for (uint i = 0; i < 3; i++)
      for (uint j = 0; j < 3; j++)
         basis[i][j] = state->basis[i][j];
   float3 attach[3][3];
   for (uint i = 0; i < 3; i++)
      for (uint j = 0; j < 3; j++)
         attach[i][j] = expand(plane[point[ident].plane[i]].plane,plane[point[ident].plane[i]].versor,basis);
   float4 position = float4(intersect(attach),1.0);
   // transform by subject and maybe object and maybe feature
   v_out.position = position;
   float3 normed[3];
   for (uint i = 0; i < 3; i++)
      normed[i] = normal(attach[i]);
   v_out.normal = half3(average3(normed));
   int face = 0;
   for (uint i = 0; i < 3; i++)
      if (point[ident].tag[i] == state->tag)
         face = point[ident].plane[i];
   int corner = 0;
   for (uint i = 0; i < 3; i++)
      if (plane[face].point[i] == ident)
         corner = i;
   v_out.color = half4(plane[face].color[corner]);
   v_out.coord = half2(plane[face].coord[corner]);
   return v_out;
}
fragment half4 fragment_main (
   VertexOutput in [[stage_in]])
{
   return in.color;
}

