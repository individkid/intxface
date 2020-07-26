struct VertexOutput {
   float4 position [[position]];
   float4 color;
   float3 normal;
   float2 coord;
};
// MAIN
vertex VertexOutput vertex_main (
   const device float3 *plane [[buffer(0)]],
   const device uint *versor [[buffer(1)]],
   const device uint3 *tag [[buffer(1)]],
   const device uint3 *index [[buffer(2)]],
   const device float4 *color [[buffer(3)]],
   const device float2 *coord [[buffer(4)]],
   const device float3x3 basis;
   const device uint tag;
   uint ident [[vertex_id]])
{
   VertexOutput v_out;
   // float3x3 point[3];
   // for (int i = 0; i < 3; i++) point[i] = expand(plane[index[ident][i]],versor[index[ident][i]],basis);
   // v_out.position = intersect(point);
   v_out.color = color[ident];
   // float3 vector[3];
   // for (int i = 0; i < 3; i++) vector[i] = normal(point[i]);
   // v_out.normal = avarage(vector,3);
   v_out.coord = coord[ident];
   return v_out;
}

