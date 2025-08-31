  type t =
    | Lambertian of { albedo : Vec3.t }
        (** A Lambertian (matte) material scatters rays with increased likelihood in directions
            closer to the surface normal. *)
    | Metal of { albedo : Vec3.t; fuzziness : float }
        (** A metal material reflects rays orthogonally to the surface normal with some degree of
            [fuzziness], or pertubation to that orthogonal direction. *)
    | Dielectric of { index : float }
        (** Technically, a dielectric material splits a ray into a reflected and refacted component.
            Here, we either reflect or refract a ray that hits such a material. The [index] is how
            much a refracted ray is bent; the greater the index, the greater the refraction, where 1
            means no refraction. *)
