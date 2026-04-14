fn main() -> Result<(), std::io::Error> {
    tonic_prost_build::configure()
        .disable_comments(&["."])
        .include_file("proto_v1_alpha18.rs")
        .compile_well_known_types(false)
        .compile_protos(             &["submodules/frequenz-api-microgrid/proto/frequenz/api/microgrid/v1alpha18/microgrid.proto"],
            &[
                "submodules/frequenz-api-microgrid/proto",
                "submodules/frequenz-api-microgrid/submodules/frequenz-api-common/proto",
                "submodules/frequenz-api-microgrid/submodules/api-common-protos",
            ],
    )
        .inspect_err(|e| {
            eprintln!("Could not compile protobuf files. Error: {:?}", e);
        })
}
