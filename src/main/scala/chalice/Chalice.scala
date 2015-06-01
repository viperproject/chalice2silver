//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

/* This file is based on the sources of Microsoft's Chalice tool, which is
 * hosted on Codeplex: http://chalice.codeplex.com/
 * The file might have changed since it has been forked, in particular, existing
 * code might have been modified or removed, and new code might have been added.
 */

package chalice


class InternalErrorException(val msg: String) extends Throwable

class NotSupportedException(val msg: String) extends Throwable

