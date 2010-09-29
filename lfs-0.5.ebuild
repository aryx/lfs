# Copyright 1999-2005 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: $

DESCRIPTION="A Logic File System"
HOMEPAGE="http://aryx.kicks-ass.org/~pad/wiki/wiki-LFS/"
SRC_URI="http://aryx.kicks-ass.org/~pad/software/project-lfs/${P}.tgz"

LICENSE="GPL-2"
SLOT="0"
KEYWORDS="amd64 x86"
IUSE="gif gtk jpg mp3 vorbis" 

DEPEND="dev-lang/ocaml
	dev-ml/camlidl
	sys-libs/db
	sys-libs/gdbm
	sys-fs/fuse
	mp3? ( dev-perl/MP3-Info dev-perl/MP3-Tag dev-perl/Compress-Zlib )
	vorbis? ( media-sound/vorbis-tools )
	gtk? ( dev-ml/lablgtk )"
RDEPEND="${DEPEND}"

src_unpack() {
    if [ "${A}" != "" ]; then
        unpack ${A}
	cd ${P}
    fi
}

src_compile() {
        econf --prefix=/usr || die "econf failed"
	emake -j1 depend all || die "emake failed"
}

src_install() {
    emake DESTDIR="${D}" install || die "emake install failed"
}

