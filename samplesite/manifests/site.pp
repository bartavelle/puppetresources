node 'test.nod' {
    apt::builddep { "glusterfs-server": }
    apt::force { "glusterfs-server":
        release => "unstable",
        version => '3.0.3',
        require => Apt::Source["debian_unstable"],
    }
    apt::pin { "karmic": priority => 700 }
    apt::pin { "karmic-updates": priority => 700 }
    apt::pin { "karmic-security": priority => 700 }
    apt::ppa { "ppa:drizzle-developers/ppa": }
    apt::source { "debian_unstable":
        location          => "http://debian.mirror.iweb.ca/debian/",
        release           => "unstable",
        repos             => "main contrib non-free",
        required_packages => "debian-keyring debian-archive-keyring",
        key               => "55BE302B",
        key_server        => "subkeys.pgp.net",
        pin               => "-10",
        include_src       => true
    }
    include gcc
    include git::gitosis
}
