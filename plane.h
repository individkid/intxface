/*
*    plane.h
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

struct Plane {
	float3 plane;
	uint versor;
	uint3 point; // match to point to choose coord and color
	float2 coord[3];
	float4 color[3];
	uint polytope; // which polytope plane is in
	uint tag; // which of 7 sets face of plane is in
};
struct Point {
	uint3 plane; // planes intersect in point
};
struct State {
	float3 basis[3][3];
	float4 subject[4];
	float4 object[4];
	float4 feature[4];
	float3 feather;
	float3 arrow;
	// float3 *cloud;
	// uint size;
	uint tag; // which planes of polytope to render this pass
	uint polytope; // whether to apply object transformation
	uint manipulate; // which plane to apply feature transformation to
};
